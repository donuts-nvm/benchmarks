!---------------------------------------------------------------------
!
! PLOTGRID: for cartesian grid generation, e.g. for plots or surfaces.
!
! Copyright (C) Dylan Jayatilaka, 1997
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
! $Id: plotgrid.foo,v 1.15.2.5 2004/04/21 09:12:56 reaper Exp $
!
!---------------------------------------------------------------------

module PLOTGRID_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: same_as_
   use REALVEC_MODULE, only: normalise_
   use REALVEC_MODULE, only: norm_
   use REALVEC_MODULE, only: quick_sort_
   use REALVEC_MODULE, only: is_zero_
   use REALVEC_MODULE, only: to_cross_product_

   use INT_MODULE, only: is_even_

   use INTVEC_MODULE, only: destroy_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: set_default_units_
   use TEXTFILE_MODULE, only: text_
   use TEXTFILE_MODULE, only: next_str_
   use TEXTFILE_MODULE, only: skip_next_item_
   use TEXTFILE_MODULE, only: next_item_
   use TEXTFILE_MODULE, only: show_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: reverted_
   use TEXTFILE_MODULE, only: read_ptr_
   use TEXTFILE_MODULE, only: read_int_

   use STR_MODULE, only: to_lower_case_

   use ATOMVEC_MODULE, only: make_shape_moments_
   use ATOMVEC_MODULE, only: bounding_box_
   use ATOMVEC_MODULE, only: centre_of_atoms_

   use REALMAT_MODULE, only: determinant_
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

   public    read_y_axis_
   interface read_y_axis_
      module procedure read_y_axis
   end interface

   public    set_bounding_box_and_axes_
   interface set_bounding_box_and_axes_
      module procedure set_bounding_box_and_axes
   end interface

   public    offset_centre_
   interface offset_centre_
      module procedure offset_centre
   end interface

   public    reset_defaults_
   interface reset_defaults_
      module procedure reset_defaults
   end interface

   public    read_offset_
   interface read_offset_
      module procedure read_offset
   end interface

   public    make_cube_of_points_
   interface make_cube_of_points_
      module procedure make_cube_of_points
      module procedure make_cube_of_points_1
   end interface

   public    read_centre_atom_
   interface read_centre_atom_
      module procedure read_centre_atom
   end interface

   public    read_x_points_
   interface read_x_points_
      module procedure read_x_points
   end interface

   public    read_kind_
   interface read_kind_
      module procedure read_kind
   end interface

   public    use_bounding_box_and_axes_
   interface use_bounding_box_and_axes_
      module procedure use_bounding_box_and_axes
   end interface

   public    read_centre_atoms_
   interface read_centre_atoms_
      module procedure read_centre_atoms
   end interface

   public    use_bounding_cube_and_axes_
   interface use_bounding_cube_and_axes_
      module procedure use_bounding_cube_and_axes
   end interface

   public    read_keywords_
   interface read_keywords_
      module procedure read_keywords
   end interface

   public    read_box_scale_factor_
   interface read_box_scale_factor_
      module procedure read_box_scale_factor
   end interface

   public    set_xyz_axes_from_x_axis_
   interface set_xyz_axes_from_x_axis_
      module procedure set_xyz_axes_from_x_axis
   end interface

   public    use_bounding_cube_
   interface use_bounding_cube_
      module procedure use_bounding_cube
   end interface

   public    set_x_axis_from_atom_list_
   interface set_x_axis_from_atom_list_
      module procedure set_x_axis_from_atom_list
   end interface

   public    make_points_
   interface make_points_
      module procedure make_points
      module procedure make_points_1
      module procedure make_points_2
   end interface

   public    process_keyword_
   interface process_keyword_
      module procedure process_keyword
   end interface

   public    put_
   interface put_
      module procedure put
   end interface

   public    read_y_width_
   interface read_y_width_
      module procedure read_y_width
   end interface

   public    read_desired_separation_
   interface read_desired_separation_
      module procedure read_desired_separation
   end interface

   public    read_x_axis_
   interface read_x_axis_
      module procedure read_x_axis
   end interface

   public    set_centre_from_atom_list_
   interface set_centre_from_atom_list_
      module procedure set_centre_from_atom_list
   end interface

   public    set_xyz_axes_from_z_axis_
   interface set_xyz_axes_from_z_axis_
      module procedure set_xyz_axes_from_z_axis
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    update_for_marching_cubes_
   interface update_for_marching_cubes_
      module procedure update_for_marching_cubes
   end interface

   public    read_x_axis_atoms_
   interface read_x_axis_atoms_
      module procedure read_x_axis_atoms
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   public    set_desired_separation_
   interface set_desired_separation_
      module procedure set_desired_separation
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    read_min_x_points_
   interface read_min_x_points_
      module procedure read_min_x_points
   end interface

   public    read_centre_
   interface read_centre_
      module procedure read_centre
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    read_z_axis_
   interface read_z_axis_
      module procedure read_z_axis
   end interface

   public    point_
   interface point_
      module procedure point
   end interface

   public    set_y_axis_from_atom_list_
   interface set_y_axis_from_atom_list_
      module procedure set_y_axis_from_atom_list
   end interface

   public    orthonormalise_y_z_to_x_axis_
   interface orthonormalise_y_z_to_x_axis_
      module procedure orthonormalise_y_z_to_x_axis
   end interface

   public    read_orbital_
   interface read_orbital_
      module procedure read_orbital
   end interface

   public    read_y_axis_atoms_
   interface read_y_axis_atoms_
      module procedure read_y_axis_atoms
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    read_junk_
   interface read_junk_
      module procedure read_junk
   end interface

   public    read_max_x_points_
   interface read_max_x_points_
      module procedure read_max_x_points
   end interface

   public    read_x_width_
   interface read_x_width_
      module procedure read_x_width
   end interface

   public    set_origin_
   interface set_origin_
      module procedure set_origin
   end interface

   public    x_or_y_axes_defined_
   interface x_or_y_axes_defined_
      module procedure x_or_y_axes_defined
   end interface

   public    set_box_scale_factor_
   interface set_box_scale_factor_
      module procedure set_box_scale_factor
   end interface

   public    use_bounding_box_
   interface use_bounding_box_
      module procedure use_bounding_box
   end interface

   public    set_bounding_box_
   interface set_bounding_box_
      module procedure set_bounding_box
   end interface

   public    set_xyz_axes_from_y_axis_
   interface set_xyz_axes_from_y_axis_
      module procedure set_xyz_axes_from_y_axis
   end interface

   public    read_units_
   interface read_units_
      module procedure read_units
   end interface

   public    read_z_axis_atoms_
   interface read_z_axis_atoms_
      module procedure read_z_axis_atoms
   end interface

   public    set_points_widths_origin_
   interface set_points_widths_origin_
      module procedure set_points_widths_origin
   end interface

   public    orthonormalise_x_y_to_z_axis_
   interface orthonormalise_x_y_to_z_axis_
      module procedure orthonormalise_x_y_to_z_axis
   end interface

   public    set_z_axis_from_atom_list_
   interface set_z_axis_from_atom_list_
      module procedure set_z_axis_from_atom_list
   end interface

   public    read_z_width_
   interface read_z_width_
      module procedure read_z_width
   end interface

contains

   subroutine create(self,atom)
    type(plotgrid_type) :: self
    ! Create object
     pointer :: self
     type(atom_type), dimension(:), pointer, optional :: atom

     nullify(self)
     allocate(self)

     call set_defaults_(self,atom)

   end subroutine

   subroutine create_copy(self,grid)
    type(plotgrid_type) :: self
    ! Create a grid object which is a duplicate of grid.
     pointer :: self
     type(plotgrid_type), intent(in) :: grid

     call create_(self)
     call copy_(self,grid)

   end subroutine

   subroutine copy(self,grid)
    type(plotgrid_type) :: self
    ! Copy the contents of grid to self.
     type(plotgrid_type), intent(in) :: grid

     self = grid

   end subroutine

   subroutine destroy(self)
    type(plotgrid_type) :: self
    ! Destroy a type(plotgrid_type) object
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

   subroutine set_defaults(self,atom)
    type(plotgrid_type) :: self
    ! Set up a default grid. If "atom" is present it is used to define an xyz
    ! bounding box for the molecule.
      type(atom_type), dimension(:), pointer, optional :: atom

      self%plot_kind      = " "
      self%orbital        = 0
      self%centre         = 0.0d0
      self%offset         = 0.0d0
      self%centre_atom    = 0
      self%x_atom_1       = 0
      self%x_atom_2       = 0
      self%y_atom_1       = 0
      self%y_atom_2       = 0
      self%x_axis(:)      = (/ 1.0d0,0.0d0,0.0d0/)
      self%y_axis(:)      = (/0.0d0, 1.0d0,0.0d0/)
      self%z_axis(:)      = (/0.0d0,0.0d0, 1.0d0/)
      self%n_x            = 75
      self%n_y            = 75
      self%n_z            = 1
      self%width(:)       = (/ 2.0d0, 2.0d0,0.0d0/)
      self%width(3)       = 0.0d0     ! assume a planar plot
      self%del            = self%width(1)/(75 - 1)
      self%x_width_set    = .false.    ! These are set false only if the widths
      self%y_width_set    = .false.    ! are explicitly read in
      self%z_width_set    = .false.
      self%box_centre     = 0.0d0
      self%bounding_box   = (/ 2.0d0, 2.0d0,0.0d0/)
      self%box_scale_factor = 1.0d0
      self%box_axes(:,1)  = (/ 1.0d0,0.0d0,0.0d0/)
      self%box_axes(:,2)  = (/0.0d0, 1.0d0,0.0d0/)
      self%box_axes(:,3)  = (/0.0d0,0.0d0, 1.0d0/)
      self%x_axis_defined = .false.
      self%y_axis_defined = .false.
      self%z_axis_defined = .false.
      self%desired_separation = 0.0d0
      nullify(self%atom)
      if (present(atom)) then
      if (associated(atom)) then
         self%atom => atom
         call set_bounding_box_and_axes_(self,atom)
      end if
      end if

   end subroutine

   subroutine reset_defaults(self)
    type(plotgrid_type) :: self
    ! Reset a default grid. The only difference to set_defaults is that the
    ! bounding box and box_axes are not set again.

      self%plot_kind      = " "
      self%orbital        = 0
      self%centre         = 0.0d0
      self%offset         = 0.0d0
      self%centre_atom    = 0
      self%x_atom_1       = 0
      self%x_atom_2       = 0
      self%y_atom_1       = 0
      self%y_atom_2       = 0
      self%n_x            = 75
      self%n_y            = 75
      self%n_z            = 1
      self%width(:)       = (/ 2.0d0, 2.0d0,0.0d0/)
      self%width(3)       = 0.0d0     ! assume a planar plot
      self%del            = self%width(1)/(75 - 1)
      self%x_width_set    = .false.    ! These are set .true. only if the widths
      self%y_width_set    = .false.    ! are explicitly read in
      self%z_width_set    = .false.
      self%x_axis_defined = .false.
      self%y_axis_defined = .false.
      self%z_axis_defined = .false.

   end subroutine

   subroutine set_bounding_box(self,atom)
    type(plotgrid_type) :: self
    ! Set up a default bounding box based on the "atom" list size.
    ! NOTE: This does not mark the widths as having been inputted.
      type(atom_type), dimension(:), pointer :: atom

   call ensure_(tonto,associated(atom),"PLOTGRID:set_bounding_box ... no atom list")
      self%box_centre   = centre_of_atoms_(atom)
      self%bounding_box = bounding_box_(atom)
      self%bounding_box = self%box_scale_factor*self%bounding_box

   end subroutine

   subroutine set_bounding_box_and_axes(self,atom)
    type(plotgrid_type) :: self
    ! Set up a default bounding box based on the "atom" list size.  NOTE: This
    ! does not mark the widths as having been inputted.  The bounding box and
    ! axes are used only with a use_bounding_box_and_axes= option.
      type(atom_type), dimension(:), pointer :: atom
      real(kind=kind(1.0d0)), dimension(3) :: sm
      real(kind=kind(1.0d0)), dimension(3,3) :: sa
      integer(kind=kind(1)), dimension(3) :: order

   call ensure_(tonto,associated(atom),"PLOTGRID:set_bounding_box_and_axes ... no atom list")
      call make_shape_moments_(atom,sm,sa)
      call quick_sort_(sm,order)
      sa = sa(:,order)
      if (determinant_(sa)<0.0d0) sa(:,3) = -sa(:,3)
      self%x_axis = sa(:,1)
      self%y_axis = sa(:,2)
      self%z_axis = sa(:,3)
      self%box_axes     = sa
      self%box_centre   = centre_of_atoms_(atom)
      self%bounding_box = bounding_box_(atom,sa)
      self%bounding_box = self%box_scale_factor*self%bounding_box

   end subroutine

   subroutine set_points_widths_origin(self)
    type(plotgrid_type) :: self
    ! Set the number of points along the x axis to be odd. Evaluate the grid
    ! point separation .del from the current x_width, .width(1); from this
    ! separation, evaluate the number of points along the y and z axes; adjust
    ! the y and z widths to be an exact multiple of .del; finally, set the origin
    ! once the widths are known.

      self%n_x = 2*(self%n_x/2) + 1                 ! Make # of axis points odd ...
      if(self%n_x>1) self%del = self%width(1)/(self%n_x-1)
      if(self%n_x==1) self%width = 0.0d0
      self%n_y  = nint(self%width(2)/self%del) + 1
      self%n_y = 2*(self%n_y/2) + 1
      self%width(2) = self%del*(self%n_y-1)             ! adjust y width to the nearest grid point
      self%n_z = nint(self%width(3)/self%del) + 1
      self%n_z = 2*(self%n_z/2) + 1
      self%width(3) = self%del*(self%n_z-1)             ! adjust z width to the nearest grid point
      self%n_pt = self%n_x*self%n_y*self%n_z
      call set_origin_(self)

   end subroutine

   subroutine set_desired_separation(self,del)
    type(plotgrid_type) :: self
    ! Set the (approximate) desired separation "del" between grid points along an
    ! axis direction.  The *actual* separation used may not be the inputted
    ! separation, but may be slightly smaller: the current .x_width is used to
    ! work out the number of points along the x_axis which is then used to work
    ! out the actual .del.  NOTE: After this routine the y and z widths are
    ! changed to be the nearest multiple of "del" (actually, ".del") that exceeds
    ! their current value.
      real(kind=kind(1.0d0)) :: del

      call ensure_(tonto,self%n_x>1,"PLOTGRID:set_desired_separation ... # of x_points must be greater than 1")
      call ensure_(tonto,del>0,"PLOTGRID:set_desired_separation ... del must be positive")
      call warn_if_(tonto,.not. self%x_width_set,"PLOTGRID:set_desired_separation ... default x_width used to calculate # of &
&x_points")
      self%desired_separation = del
      self%n_x = ceiling(self%width(1)/del)
      call set_points_widths_origin_(self)

   end subroutine

   subroutine set_centre_from_atom_list(self,atom)
    type(plotgrid_type) :: self
    ! Set the .centre of the grid data from an "atom" list. The .origin of the
    ! plot isd also set using the current plot widths and plot centre.
      type(atom_type), dimension(:), pointer :: atom

   call ensure_(tonto,associated(atom),"PLOTGRID:set_centre_from_atom_list ... atom list not present")
   call ensure_(tonto,self%centre_atom/=0,"PLOTGRID:set_centre_from_atom_list ... no centre_atom")
      self%centre = atom(self%centre_atom)%pos
      call set_origin_(self)

   end subroutine

   subroutine offset_centre(self)
    type(plotgrid_type) :: self
    ! Offset the .centre by .offset along the current axes.

      self%centre = self%centre + self%offset(1)*self%x_axis
      self%centre = self%centre + self%offset(2)*self%y_axis
      self%centre = self%centre + self%offset(3)*self%z_axis
      call set_origin_(self)

   end subroutine

   subroutine set_origin(self)
    type(plotgrid_type) :: self
    ! Set the origin of the plot (the bottom, front, left-hand corner). It is
    ! equal to the .centre of the plot minus half the (current) .widths along the
    ! (current) x, y, and z axes.

      self%origin = self%centre
      self%origin = self%origin -self%width(1)*0.50d0*self%x_axis
      self%origin = self%origin -self%width(2)*0.50d0*self%y_axis
      self%origin = self%origin -self%width(3)*0.50d0*self%z_axis

   end subroutine

   subroutine set_x_axis_from_atom_list(self,atom)
    type(plotgrid_type) :: self
    ! Set the x-axis of the grid data from an "atom" list. If the .x_width has
    ! not been explicitly set, it is set to twice the length between the atom
    ! separation (because probably the user wants to see at least those atoms in
    ! his plot!).
      type(atom_type), dimension(:), pointer :: atom

   call ensure_(tonto,associated(atom),"PLOTGRID:set_x_axis_from_atom_list ... atom list not present")
   call ensure_(tonto,self%x_atom_1/=0,"PLOTGRID:set_x_axis_from_atom_list ... no x_atom_1")
   call ensure_(tonto,self%x_atom_2/=0,"PLOTGRID:set_x_axis_from_atom_list ... no x_atom_2")
      self%x_axis = atom(self%x_atom_2)%pos - atom(self%x_atom_1)%pos
      if (.not. self%x_width_set) then
         self%width(1) = 2.0d0*norm_(self%x_axis)
         call set_points_widths_origin_(self)
      end if
      call set_xyz_axes_from_x_axis_(self)

   end subroutine

   subroutine set_y_axis_from_atom_list(self,atom)
    type(plotgrid_type) :: self
    ! Set the y-axis of the grid data from an "atom" list. If the .y_width has
    ! not been explicitly set, it is set to twice the length between the atom
    ! separation (because probably the user wants to see at least those atoms in
    ! his plot!).
      type(atom_type), dimension(:), pointer :: atom

   call ensure_(tonto,associated(atom),"PLOTGRID:set_y_axis_from_atom_list ... atom list not present")
   call ensure_(tonto,self%y_atom_1/=0,"PLOTGRID:set_y_axis_from_atom_list ... no y_atom_1")
   call ensure_(tonto,self%y_atom_2/=0,"PLOTGRID:set_y_axis_from_atom_list ... no y_atom_2")
      self%y_axis = atom(self%y_atom_2)%pos - atom(self%y_atom_1)%pos
      if (.not. self%y_width_set) then
         self%width(2) = 2.0d0*norm_(self%y_axis)
         call set_points_widths_origin_(self)
      end if
      call set_xyz_axes_from_y_axis_(self)

   end subroutine

   subroutine set_z_axis_from_atom_list(self,atom)
    type(plotgrid_type) :: self
    ! Set the z-axis of the grid data from an "atom" list. If the .z_width has
    ! not been explicitly set, it is set to twice the length between the atom
    ! separation (because probably the user wants to see at least those atoms in
    ! his plot!).
      type(atom_type), dimension(:), pointer :: atom

   call ensure_(tonto,associated(atom),"PLOTGRID:set_z_axis_from_atom_list ... atom list not present")
   call ensure_(tonto,self%z_atom_1/=0,"PLOTGRID:set_z_axis_from_atom_list ... no z_atom_1")
   call ensure_(tonto,self%z_atom_2/=0,"PLOTGRID:set_z_axis_from_atom_list ... no z_atom_2")
      self%z_axis = atom(self%z_atom_2)%pos - atom(self%z_atom_1)%pos
      if (.not. self%z_width_set) then
         self%width(3) = 2.0d0*norm_(self%z_axis)
         call set_points_widths_origin_(self)
      end if
      call set_xyz_axes_from_z_axis_(self)

   end subroutine

   subroutine set_xyz_axes_from_x_axis(self)
    type(plotgrid_type) :: self
    ! Set the x,y,z axes of the grid given a new x_axis vector. Also set the
    ! origin since that is dependent on the axes.

   call ensure_(tonto,.not. self%x_axis_defined,"PLOTGRID:set_xyz_axes_from_x_axis ... x_axis already explicitly defined")
   call ensure_(tonto,.not. self%y_axis_defined,"PLOTGRID:set_xyz_axes_from_x_axis ... define x_axis before y_axis")
   call warn_if_(tonto,self%z_axis_defined,"PLOTGRID:set_xyz_axes_from_x_axis ... orthonormalising inputted x_axis and y_axis&
& to z_axis")
      call normalise_(self%x_axis)
      if (self%z_axis_defined) then; call orthonormalise_x_y_to_z_axis_(self)
      else;                      call orthonormalise_y_z_to_x_axis_(self)
      end if
      call set_origin_(self)

   end subroutine

   subroutine set_xyz_axes_from_y_axis(self)
    type(plotgrid_type) :: self
    ! Set the x,y,z axes of the grid given a new y_axis vector. Also set the
    ! origin since that is dependent on the axes.

   call ensure_(tonto,.not. self%y_axis_defined,"PLOTGRID:set_xyz_axes_from_y_axis ... y_axis already explicitly defined")
   call ensure_(tonto,.not. self%z_axis_defined,"PLOTGRID:set_xyz_axes_from_y_axis ... can't set y_axis: z_axis and x_axis ar&
&e already defined")
   call warn_(tonto,"PLOTGRID:set_xyz_axes_from_y_axis ... y_axis is *always* orthonormalised to x_axis")
      call normalise_(self%y_axis)
      call die_if_(tonto,same_as_(self%y_axis,self%x_axis),"PLOTGRID:set_xyz_axes_from_y_axis ... y_axis is the same as x_axi&
&s")
      call orthonormalise_y_z_to_x_axis_(self)
      call set_origin_(self)

   end subroutine

   subroutine set_xyz_axes_from_z_axis(self)
    type(plotgrid_type) :: self
    ! Set the x,y,z axes of the grid given a new z_axis vector. Also set the
    ! origin since that is dependent on the axes.

   call ensure_(tonto,.not. self%z_axis_defined,"PLOTGRID:set_xyz_axes_from_z_axis ... z_axis already explicitly defined")
   call warn_if_(tonto,x_or_y_axes_defined_(self),"PLOTGRID:set_xyz_axes_from_z_axis ... inputted x_axis, y_axis to be orthon&
&ormalised to inputted z_axis")
      call normalise_(self%z_axis)
      call orthonormalise_x_y_to_z_axis_(self)
      call set_origin_(self)

   end subroutine

   function x_or_y_axes_defined(self) result(res)
    type(plotgrid_type) :: self
    ! Return .true. if either the x or y axes have been explicitly inputted.
      logical(kind=kind(.true.)) :: res

      res = self%x_axis_defined .or. self%y_axis_defined

   end function

   subroutine orthonormalise_x_y_to_z_axis(self)
    type(plotgrid_type) :: self
    ! Orthogonalise the x and y axes to the z axis.
      real(kind=kind(1.0d0)) :: dot

      if (same_as_(self%x_axis,self%z_axis)) &
         self%x_axis = self%y_axis                    ! Make sure x and z are different
      dot = dot_product(self%z_axis,self%x_axis)      ! Orthogonalise x_axis to z_axis
      self%x_axis = self%x_axis - dot*self%z_axis
      call normalise_(self%x_axis)
      call die_if_(tonto,is_zero_(self%x_axis),"PLOTGRID:orthonormalise_x_y_to_z_axis ... x_axis is same as z_axis!")
      call to_cross_product_(self%y_axis,self%z_axis,self%x_axis)

   end subroutine

   subroutine orthonormalise_y_z_to_x_axis(self)
    type(plotgrid_type) :: self
    ! Orthogonalise the x and y axes to the z axis.
      real(kind=kind(1.0d0)) :: dot

      if (same_as_(self%y_axis,self%x_axis)) &
         self%y_axis = self%z_axis                    ! Make sure y and x are different
      dot = dot_product(self%x_axis,self%y_axis)      ! Orthogonalise y_axis to x_axis
      self%y_axis = self%y_axis - dot*self%x_axis
      call normalise_(self%y_axis)
      call die_if_(tonto,is_zero_(self%y_axis),"PLOTGRID:orthonormalise_y_z_to_x_axis ... y_axis is same as x_axis!")
      call to_cross_product_(self%z_axis,self%x_axis,self%y_axis)

   end subroutine

   subroutine update_for_marching_cubes(self)
    type(plotgrid_type) :: self
    ! Update the grid data to be consistent with the (non-recursive) marching
    ! cubes algorithm. Essentially, a gridpoint is added along all dimensions.

      call warn_(tonto,"PLOTGRID:update_for_marching_cubes ... adjusting grid by adding 2 extra points on all sides")
      self%n_x = self%n_x + 2
      self%n_y = self%n_y + 2
      self%n_z = self%n_z + 2
      call ensure_(tonto,self%n_x>=4,"PLOTGRID:update_for_marching_cubes ... not enough x points for marching cube isosurface&
& algorithm")
      call ensure_(tonto,self%n_y>=4,"PLOTGRID:update_for_marching_cubes ... not enough y points for marching cube isosurface&
& algorithm")
      call ensure_(tonto,self%n_z>=4,"PLOTGRID:update_for_marching_cubes ... not enough z points for marching cube isosurface&
& algorithm")
       ! center remains the same, extra points added around edges
      self%width  = self%width  + 2*self%del
      self%origin = self%origin - self%del*self%x_axis
      self%origin = self%origin - self%del*self%y_axis
      self%origin = self%origin - self%del*self%z_axis
      self%n_pt = self%n_x*self%n_y*self%n_z

   end subroutine

!  *************
!  Input methods
!  *************

   subroutine read_keywords(self)
    type(plotgrid_type) :: self
    ! Read data from "stdin" using keyword style input.
    ! The following code is inherited from OBJECT
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","PLOTGRID:read_keywords ... expecting open bracket symbol, {")
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
    type(plotgrid_type) :: self
    ! Process command "keyword". Any required data needed by the "keyword" is
    ! inputted from "stdin".
      character(*) :: keyword
      character(128) :: word

      word = keyword
      call to_lower_case_(word)
      select case (word)
         case ("}                         ")   ! exit case
         case ("box_scale_factor=         "); call read_box_scale_factor_(self)
         case ("centre=                   "); call read_centre_(self)
         case ("centre_atom=              "); call read_centre_atom_(self)
         case ("centre_atoms=             "); call read_centre_atoms_(self)
         case ("crystal_plane=            "); call read_z_axis_(self)
         case ("desired_separation=       "); call read_desired_separation_(self)
         case ("kind=                     "); call read_kind_(self)
         case ("max_x_points=             "); call read_max_x_points_(self)
         case ("min_x_points=             "); call read_min_x_points_(self)
         case ("n_points=                 "); call read_x_points_(self)
         case ("normal=                   "); call read_z_axis_(self)
         case ("offset=                   "); call read_offset_(self)
         case ("orbital=                  "); call read_orbital_(self)
         case ("put                       "); call put_(self)
         case ("units=                    "); call read_units_(self)
         case ("use_bounding_box          "); call use_bounding_box_(self)
         case ("use_bounding_box_and_axes "); call use_bounding_box_and_axes_(self)
         case ("use_bounding_cube         "); call use_bounding_cube_(self)
         case ("use_bounding_cube_and_axes"); call use_bounding_cube_and_axes_(self)
         case ("x_axis=                   "); call read_x_axis_(self)
         case ("x_axis_atoms=             "); call read_x_axis_atoms_(self)
         case ("x_points=                 "); call read_x_points_(self)
         case ("x_width=                  "); call read_x_width_(self)
         case ("y_axis=                   "); call read_y_axis_(self)
         case ("y_axis_atoms=             "); call read_y_axis_atoms_(self)
         case ("y_width=                  "); call read_y_width_(self)
         case ("z_axis=                   "); call read_z_axis_(self)
         case ("z_width=                  "); call read_z_width_(self)
         case default;                    allocate(tonto%known_keywords(29))
         tonto%known_keywords(1) = "}                         "
         tonto%known_keywords(2) = "box_scale_factor=         "
         tonto%known_keywords(3) = "centre=                   "
         tonto%known_keywords(4) = "centre_atom=              "
         tonto%known_keywords(5) = "centre_atoms=             "
         tonto%known_keywords(6) = "crystal_plane=            "
         tonto%known_keywords(7) = "desired_separation=       "
         tonto%known_keywords(8) = "kind=                     "
         tonto%known_keywords(9) = "max_x_points=             "
         tonto%known_keywords(10) = "min_x_points=             "
         tonto%known_keywords(11) = "n_points=                 "
         tonto%known_keywords(12) = "normal=                   "
         tonto%known_keywords(13) = "offset=                   "
         tonto%known_keywords(14) = "orbital=                  "
         tonto%known_keywords(15) = "put                       "
         tonto%known_keywords(16) = "units=                    "
         tonto%known_keywords(17) = "use_bounding_box          "
         tonto%known_keywords(18) = "use_bounding_box_and_axes "
         tonto%known_keywords(19) = "use_bounding_cube         "
         tonto%known_keywords(20) = "use_bounding_cube_and_axes"
         tonto%known_keywords(21) = "x_axis=                   "
         tonto%known_keywords(22) = "x_axis_atoms=             "
         tonto%known_keywords(23) = "x_points=                 "
         tonto%known_keywords(24) = "x_width=                  "
         tonto%known_keywords(25) = "y_axis=                   "
         tonto%known_keywords(26) = "y_axis_atoms=             "
         tonto%known_keywords(27) = "y_width=                  "
         tonto%known_keywords(28) = "z_axis=                   "
         tonto%known_keywords(29) = "z_width=                  "
         call unknown_(tonto,word,"PLOTGRID:process_keyword")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_units(self)
    type(plotgrid_type) :: self
    ! Read a string which describes the units to be used
    ! The following code is inherited from OBJECT

      call set_default_units_(stdin,next_str_(stdin))

   end subroutine

   subroutine read_junk(self)
    type(plotgrid_type) :: self
    ! Read in a junk string, useful for ignoring a field
    ! The following code is inherited from OBJECT

      call skip_next_item_(stdin)

   end subroutine

   subroutine read_kind(self)
    type(plotgrid_type) :: self
    ! Read in the plot kind

      call read_(stdin,self%plot_kind)
      select case(self%plot_kind)
         case("crystal_error_map  ")
         case("electron_density   ")
         case("laplacian_density  ")
         case("orbital_density    ")
         case("orbital            ")
         case("difference_density ")
         case("true_fermi_mobility")
         case("fermi_mobility     ")
         case("qq_plot            ")
         case("spin_density       ")
         case("current_density    ")
         case("j_density          ")
         case("jp_density         ")
         case("div_jp_density     ")
         case("jd_density         ")
         case("elf                ")
         case("tsirelson_elf      ")
         case("electric_potential ")
         case("solenoidal_jp      ")
         case("hirshfeld_density  ")
         case("stockholder_density")
         case default;              allocate(tonto%known_keywords(21))
         tonto%known_keywords(1) = "crystal_error_map  "
         tonto%known_keywords(2) = "electron_density   "
         tonto%known_keywords(3) = "laplacian_density  "
         tonto%known_keywords(4) = "orbital_density    "
         tonto%known_keywords(5) = "orbital            "
         tonto%known_keywords(6) = "difference_density "
         tonto%known_keywords(7) = "true_fermi_mobility"
         tonto%known_keywords(8) = "fermi_mobility     "
         tonto%known_keywords(9) = "qq_plot            "
         tonto%known_keywords(10) = "spin_density       "
         tonto%known_keywords(11) = "current_density    "
         tonto%known_keywords(12) = "j_density          "
         tonto%known_keywords(13) = "jp_density         "
         tonto%known_keywords(14) = "div_jp_density     "
         tonto%known_keywords(15) = "jd_density         "
         tonto%known_keywords(16) = "elf                "
         tonto%known_keywords(17) = "tsirelson_elf      "
         tonto%known_keywords(18) = "electric_potential "
         tonto%known_keywords(19) = "solenoidal_jp      "
         tonto%known_keywords(20) = "hirshfeld_density  "
         tonto%known_keywords(21) = "stockholder_density"
         call unknown_(tonto,self%plot_kind,"PLOTGRID:read_kind")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_desired_separation(self)
    type(plotgrid_type) :: self
    ! Read the (approximate) desired separation between grid points along an axis
    ! direction.  The number of x points for the plot is adjusted so that the
    ! *actual* separation is less than the inputted separation. NOTE: the current
    ! .x_width is used to calculate the number of points.
      real(kind=kind(1.0d0)) :: del

      call ensure_(tonto,self%n_x>1,"PLOTGRID:read_desired_separation ... # of x_points must be greater than 1")
      call warn_if_(tonto,.not. self%x_width_set,"PLOTGRID:read_desired_separation ... default x_width used to calculate # of&
& x_points")
      call read_(stdin,del)
      call set_desired_separation_(self,del)

   end subroutine

   subroutine read_max_x_points(self)
    type(plotgrid_type) :: self
    ! Read the *maximum number* of x points to be used for the plot.
    ! The number of points is adjusted to be no more than this.
      integer(kind=kind(1)) :: max_n_x

      call read_(stdin,max_n_x)
      if (self%n_x<=max_n_x) then;   return; end if
      self%n_x = max_n_x
      call warn_if_(tonto,is_even_(self%n_x),"PLOTGRID:read_max_x_points ... resetting to nearest odd number")
      call set_points_widths_origin_(self)

   end subroutine

   subroutine read_min_x_points(self)
    type(plotgrid_type) :: self
    ! Read the *minimum number* of x points to be used for the plot.
      integer(kind=kind(1)) :: min_n_x

      call read_(stdin,min_n_x)
      if (min_n_x<=self%n_x) then;   return; end if
      self%n_x = min_n_x
      call warn_if_(tonto,is_even_(self%n_x),"PLOTGRID:read_min_x_points ... resetting to nearest odd number")
      call set_points_widths_origin_(self)

   end subroutine

   subroutine read_x_points(self)
    type(plotgrid_type) :: self
    ! Read the number of x points for the plot. This is the precision
    ! of the plot.

      call read_int_(stdin,self%n_x)
      call ensure_(tonto,self%n_x>0,"PLOTGRID:read_x_points ... number of x_points must be positive")
      call warn_if_(tonto,is_even_(self%n_x),"PLOTGRID:read_x_points ... resetting to nearest odd number")
      call set_points_widths_origin_(self)

   end subroutine

   subroutine read_offset(self)
    type(plotgrid_type) :: self
    ! Read a 3-vector, used to offset the plot along.

      call read_(stdin,self%offset)
      call offset_centre_(self)

   end subroutine

   subroutine read_orbital(self)
    type(plotgrid_type) :: self
    ! Read the orbital to plot out

      call read_int_(stdin,self%orbital)

   end subroutine

   subroutine read_centre_atom(self)
    type(plotgrid_type) :: self
    ! Read the centre atom of the plot

      call read_(stdin,self%centre_atom)
      call set_centre_from_atom_list_(self,self%atom)

   end subroutine

   subroutine read_centre_atoms(self)
    type(plotgrid_type) :: self
    ! Make the centre of the plot to be the centre of the list of atom indices.
      integer(kind=kind(1)), dimension(:), pointer :: atom_list
      integer(kind=kind(1)) :: i

      call ensure_(tonto,associated(self%atom),"PLOTGRID:read_centre_atoms ... atom list not present")
      call read_ptr_(stdin,atom_list)
      call ensure_(tonto,size(atom_list)>0,"PLOTGRID:read_centre_atoms ... no atoms specified in input")
      self%centre = 0.0d0
      do i=1,size(atom_list)
        self%centre = self%centre + self%atom(atom_list(i))%pos
      end do
      self%centre = self%centre / size(atom_list)
      call destroy_(atom_list)
      call set_origin_(self)

   end subroutine

   subroutine read_centre(self)
    type(plotgrid_type) :: self
    ! Read the centre position of the plot. Also adjust the origin of the plot.

      call read_(stdin,self%centre)
      call set_origin_(self)

   end subroutine

   subroutine read_x_axis(self)
    type(plotgrid_type) :: self
    ! Read the x axis vector of the plot. Normally, the y and z axes are defined
    ! to be orthogonal to the inputted axes, based on the defualt axis settings.
    ! However, if the z axis has been explicitly defined before hand, then the x
    ! and y axes are orthogonalised to it.

      call read_(stdin,self%x_axis)
      call set_xyz_axes_from_x_axis_(self)
      self%x_axis_defined = .true.

   end subroutine

   subroutine read_x_axis_atoms(self)
    type(plotgrid_type) :: self
    ! Read the atoms which define the x axis vector of the plot. If the z axis
    ! has been defined, then the x_axis is orthogonalised to it. Otherwise the y
    ! and z axes are orthogonalised to this vector.

      call read_int_(stdin,self%x_atom_1)
      call read_int_(stdin,self%x_atom_2)
      call die_if_(tonto,self%x_atom_1==self%x_atom_2,"PLOTGRID:read_x_axis_atoms ... cannot specify same x axis atoms")
      call set_x_axis_from_atom_list_(self,self%atom)
      self%x_axis_defined = .true.

   end subroutine

   subroutine read_y_axis(self)
    type(plotgrid_type) :: self
    ! Read the y axis vector of the plot. It is an error to use this command if
    ! the .z_axis has already been inputted, because the .y_axis is made
    ! orthogonal to the x_axis and z_axis. Even if the z_axis has not been
    ! defined, the y_axis is made orthogonal to the x_axis.

      call read_(stdin,self%y_axis)
      call set_xyz_axes_from_y_axis_(self)
      self%y_axis_defined = .true.

   end subroutine

   subroutine read_y_axis_atoms(self)
    type(plotgrid_type) :: self
    ! Read the atoms which define the y axis vector of the plot. It is an error
    ! to use this command if the .z_axis has already been inputted, because the
    ! .y_axis is made orthogonal to the x_axis and z_axis. Even if the z_axis has
    ! not been

      call read_int_(stdin,self%y_atom_1)
      call read_int_(stdin,self%y_atom_2)
      call die_if_(tonto,self%y_atom_1==self%y_atom_2,"PLOTGRID:read_y_axis_atoms ... cannot specify same y axis atoms")
      call set_y_axis_from_atom_list_(self,self%atom)
      self%y_axis_defined = .true.

   end subroutine

   subroutine read_z_axis(self)
    type(plotgrid_type) :: self
    ! Read the z axis of the plot. NOTE: If it is inputted, the current x and y
    ! axes are made orthogonal to it.

      call read_(stdin,self%z_axis)
      call set_xyz_axes_from_z_axis_(self)
      self%z_axis_defined = .true.

   end subroutine

   subroutine read_z_axis_atoms(self)
    type(plotgrid_type) :: self
    ! Read the atoms which define the z axis vector of the plot.

      call read_int_(stdin,self%z_atom_1)
      call read_int_(stdin,self%z_atom_2)
      call die_if_(tonto,self%z_atom_1==self%z_atom_2,"PLOTGRID:read_z_axis_atoms ... cannot specify same z axis atoms")
      call set_z_axis_from_atom_list_(self,self%atom)
      self%z_axis_defined = .true.

   end subroutine

   subroutine read_x_width(self)
    type(plotgrid_type) :: self
    ! Read the x width of the plot

      call read_(stdin,self%width(1))
      call set_points_widths_origin_(self)
      self%x_width_set = .true.

   end subroutine

   subroutine read_y_width(self)
    type(plotgrid_type) :: self
    ! Read the y width of the plot

      call read_(stdin,self%width(2))
      call set_points_widths_origin_(self)
      self%y_width_set = .true.

   end subroutine

   subroutine read_z_width(self)
    type(plotgrid_type) :: self
    ! Read the z width of the plot. This is normally 0.

      call read_(stdin,self%width(3))
      call set_points_widths_origin_(self)
      self%z_width_set = .true.

   end subroutine

   subroutine read_box_scale_factor(self)
    type(plotgrid_type) :: self
    ! Read the bounding box scale factor and apply it immediately.

      call read_(stdin,self%box_scale_factor)
      call ensure_(tonto,self%box_scale_factor>0.0d0,"PLOTGRID:read_box_scale_factor ... scale factor not positive")

   end subroutine

   subroutine set_box_scale_factor(self,factor)
    type(plotgrid_type) :: self
    ! Set the bounding box scale factor
      real(kind=kind(1.0d0)) :: factor

      self%box_scale_factor = factor
      call ensure_(tonto,self%box_scale_factor>0.0d0,"PLOTGRID:set_box_scale_factor ... scale factor not positive")

   end subroutine

   subroutine use_bounding_box(self)
    type(plotgrid_type) :: self
    ! Use the default bounding box centre and bounding box widths.

       call set_bounding_box_(self,self%atom)
       self%centre = self%box_centre
       self%width  = self%bounding_box
       call set_points_widths_origin_(self)

   end subroutine

   subroutine use_bounding_cube(self)
    type(plotgrid_type) :: self
    ! Use the default bounding box centre and bounding box widths.

       call set_bounding_box_(self,self%atom)
       self%centre = self%box_centre
       self%width  = maxval(self%bounding_box)
       call set_points_widths_origin_(self)

   end subroutine

   subroutine use_bounding_box_and_axes(self)
    type(plotgrid_type) :: self
    ! Use the default bounding box centre, bounding box axes, and bounding box
    ! widths.

       call set_bounding_box_and_axes_(self,self%atom)
       self%centre = self%box_centre
       self%width  = self%bounding_box
       self%x_axis = self%box_axes(:,1)
       self%y_axis = self%box_axes(:,2)
       self%z_axis = self%box_axes(:,3)
       call set_points_widths_origin_(self)

   end subroutine

   subroutine use_bounding_cube_and_axes(self)
    type(plotgrid_type) :: self
    ! Use the default bounding box centre, bounding box axes, and bounding box
    ! widths.

       call set_bounding_box_and_axes_(self,self%atom)
       self%centre = self%box_centre
       self%width  = maxval(self%bounding_box)
       self%x_axis = self%box_axes(:,1)
       self%y_axis = self%box_axes(:,2)
       self%z_axis = self%box_axes(:,3)
       call set_points_widths_origin_(self)

   end subroutine

!   widths_were_set result (res)
!   ! Return .true. if any of the widths were inputted.
!      res :: logical(kind=kind(.true.))
!      res =  .x_width_set &
!          .or. .y_width_set &
!          .or. .z_width_set
!   end

!   read_crystal_plane(in,unitcell)
!   ! To read in a vector which describes a plane in the crystal.
!   ! The vector is the normal of the plane.
!     in :: INPUT
!     unitcell :: type(unitcell_type), intent(in)
!     in.read( .z_axis )
!     .z_axis.rotate(unitcell.cell_matrix)
!     .z_axis.normalise
!     .x_axis.normalise
!     .y_axis.to_cross_product( .z_axis, .x_axis)
!     .y_axis.normalise
!     .x_axis.to_cross_product( .y_axis, .z_axis)
!     .x_axis.normalise
!   end

   subroutine make_points(self,x_pt,y_pt,z_pt)
    type(plotgrid_type) :: self
    ! Make a list of the grid points
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:), intent(out) :: x_pt,y_pt,z_pt
      real(kind=kind(1.0d0)) :: x1,x2,x3,y1,y2,y3,z1,z2,z3
      real(kind=kind(1.0d0)) :: ox,oy,oz
      integer(kind=kind(1)) :: ix,iy,iz,i_pt,t2,t3

   call ensure_(tonto,size(x_pt) == self%n_pt,"PLOTGRID:make_points ... array of points not correct size")
   call ensure_(tonto,size(y_pt) == self%n_pt,"PLOTGRID:make_points ... array of points not correct size")
   call ensure_(tonto,size(z_pt) == self%n_pt,"PLOTGRID:make_points ... array of points not correct size")
      ox = self%origin(1);      oy = self%origin(2);      oz = self%origin(3)
      x1 = self%del*self%x_axis(1); x2 = self%del*self%x_axis(2); x3 = self%del*self%x_axis(3)
      y1 = self%del*self%y_axis(1); y2 = self%del*self%y_axis(2); y3 = self%del*self%y_axis(3)
      z1 = self%del*self%z_axis(1); z2 = self%del*self%z_axis(2); z3 = self%del*self%z_axis(3)
      t2 = self%n_x*self%n_y
      do i_pt = 1,self%n_pt
         iz = (i_pt-1)/t2
         t3 = i_pt - iz*t2
         iy = (t3-1)/self%n_x
         ix = t3 - iy*self%n_x
         x_pt(i_pt) = ox + ix*x1 + iy*y1 + iz*z1
         y_pt(i_pt) = oy + ix*x2 + iy*y2 + iz*z2
         z_pt(i_pt) = oz + ix*x3 + iy*y3 + iz*z3
      end do

   end subroutine

   subroutine make_points_1(self,pt,first_pt,last_pt)
    type(plotgrid_type) :: self
    ! Return a list of the grid points in "pt". Will return a subset of the grid points
    ! from "first_pt" to "last_pt", if these options are provided.
      intent(in) :: self
      integer(kind=kind(1)), intent(in), optional :: first_pt,last_pt
      real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: pt
      real(kind=kind(1.0d0)) :: x1,x2,x3,y1,y2,y3,z1,z2,z3
      real(kind=kind(1.0d0)) :: ox,oy,oz
      integer(kind=kind(1)) :: ix,iy,iz,i_pt,first,last,t2,t3

      call ensure_(tonto,self%n_x/=0,"PLOTGRID:make_points_1 ... zero number of x points")
      call ensure_(tonto,self%n_y/=0,"PLOTGRID:make_points_1 ... zero number of y points")
      first = 1;          last = size(pt,1)
      if (present(first_pt))     first = first_pt
      if (present(last_pt))      last  = last_pt
      ox = self%origin(1);      oy = self%origin(2);      oz = self%origin(3)
      x1 = self%del*self%x_axis(1); x2 = self%del*self%x_axis(2); x3 = self%del*self%x_axis(3)
      y1 = self%del*self%y_axis(1); y2 = self%del*self%y_axis(2); y3 = self%del*self%y_axis(3)
      z1 = self%del*self%z_axis(1); z2 = self%del*self%z_axis(2); z3 = self%del*self%z_axis(3)
      t2 = self%n_x*self%n_y
      do i_pt = first,last
         iz = (i_pt-1)/t2
         t3 = i_pt - iz*t2
         iy = (t3-1)/self%n_x
         ix = t3 - iy*self%n_x
         pt(i_pt,1) = ox + ix*x1 + iy*y1 + iz*z1
         pt(i_pt,2) = oy + ix*x2 + iy*y2 + iz*z2
         pt(i_pt,3) = oz + ix*x3 + iy*y3 + iz*z3
      end do

   end subroutine

   subroutine make_points_2(self,pt,fx,lx,fy,ly,fz,lz)
    type(plotgrid_type) :: self
    ! Return a list of the grid points in "pt" in grid xyz order, starting from
    ! the first and last x, y, and z points: "fx", "lx", "fy", "ly", "fz", "lz"
    ! respectively
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: pt
      integer(kind=kind(1)), intent(in) :: fx,lx,fy,ly,fz,lz
      real(kind=kind(1.0d0)) :: x1,x2,x3,y1,y2,y3,z1,z2,z3
      real(kind=kind(1.0d0)) :: ox,oy,oz
      integer(kind=kind(1)) :: x,y,z,ix,iy,iz,i_pt

   call ensure_(tonto,size(pt,1)==(lx-fx+1)*(ly-fy+1)*(lz-fz+1),"PLOTGRID:make_points_2 ... wrong 1st dimension, pt")
   call ensure_(tonto,size(pt,2)==3,"PLOTGRID:make_points_2 ... wrong 2nd dimension, pt")
      ox = self%origin(1);      oy = self%origin(2);      oz = self%origin(3)
      x1 = self%del*self%x_axis(1); x2 = self%del*self%x_axis(2); x3 = self%del*self%x_axis(3)
      y1 = self%del*self%y_axis(1); y2 = self%del*self%y_axis(2); y3 = self%del*self%y_axis(3)
      z1 = self%del*self%z_axis(1); z2 = self%del*self%z_axis(2); z3 = self%del*self%z_axis(3)
      i_pt = 0
      do z = fz,lz
      do y = fy,ly
      do x = fx,lx
         ix = x - 1
         iy = y - 1
         iz = z - 1
         i_pt = i_pt + 1
         pt(i_pt,1) = ox + ix*x1 + iy*y1 + iz*z1
         pt(i_pt,2) = oy + ix*x2 + iy*y2 + iz*z2
         pt(i_pt,3) = oz + ix*x3 + iy*y3 + iz*z3
      end do
      end do
      end do

   end subroutine

   function point(self,x,y,z) result(res)
    type(plotgrid_type) :: self
    ! Return one particular point "res" of the grid, given by grid coordinates
    ! "x", "y", and "z". (1,1,1) is the bottom (front) left hand corner.
      intent(in) :: self
      integer(kind=kind(1)), intent(in) :: x,y,z
      real(kind=kind(1.0d0)), dimension(3) :: res
      real(kind=kind(1.0d0)) :: x1,x2,x3,y1,y2,y3,z1,z2,z3,ox,oy,oz
      integer(kind=kind(1)) :: ix,iy,iz

      ox = self%origin(1);      oy = self%origin(2);      oz = self%origin(3)
      x1 = self%del*self%x_axis(1); x2 = self%del*self%x_axis(2); x3 = self%del*self%x_axis(3)
      y1 = self%del*self%y_axis(1); y2 = self%del*self%y_axis(2); y3 = self%del*self%y_axis(3)
      z1 = self%del*self%z_axis(1); z2 = self%del*self%z_axis(2); z3 = self%del*self%z_axis(3)
      ix = x - 1
      iy = y - 1
      iz = z - 1
         res(1) = ox + ix*x1 + iy*y1 + iz*z1
         res(2) = oy + ix*x2 + iy*y2 + iz*z2
         res(3) = oz + ix*x3 + iy*y3 + iz*z3

   end function

   subroutine make_cube_of_points(self,p,x,y,z)
    type(plotgrid_type) :: self
    ! Return a cube of 8 grid points "p", where the bottom (front) left hand
    ! corner point has grid coordinates "x", "y", and "z".
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(3,0:1,0:1,0:1) :: p
      integer(kind=kind(1)) :: x,y,z
      real(kind=kind(1.0d0)) :: x1,x2,x3,y1,y2,y3,z1,z2,z3,ox,oy,oz
      integer(kind=kind(1)) :: ix,iy,iz

      x1 = self%del*self%x_axis(1); x2 = self%del*self%x_axis(2); x3 = self%del*self%x_axis(3)
      y1 = self%del*self%y_axis(1); y2 = self%del*self%y_axis(2); y3 = self%del*self%y_axis(3)
      z1 = self%del*self%z_axis(1); z2 = self%del*self%z_axis(2); z3 = self%del*self%z_axis(3)
      ix = x - 1
      iy = y - 1
      iz = z - 1
      ox = self%origin(1) + ix*x1 + iy*y1 + iz*z1  ! cube origin
      oy = self%origin(2) + ix*x2 + iy*y2 + iz*z2
      oz = self%origin(3) + ix*x3 + iy*y3 + iz*z3
      do iz = 0,1
      do iy = 0,1
      do ix = 0,1
         p(1,ix,iy,iz) = ox + ix*x1 + iy*y1 + iz*z1
         p(2,ix,iy,iz) = oy + ix*x2 + iy*y2 + iz*z2
         p(3,ix,iy,iz) = oz + ix*x3 + iy*y3 + iz*z3
      end do
      end do
      end do

   end subroutine

   subroutine make_cube_of_points_1(self,p,f,l,del,x,y,z)
    type(plotgrid_type) :: self
    ! Return a cube of points "p" where the cube edge points start from index "f"
    ! and end at index "l", and each point separated by distance "del" along a
    ! grid axis direction ("del" *not* the same as the grid .del).  The (0,0,0)
    ! point of the cube corresponds to the plotgrid coordinates "x", "y", "z",
    ! which are numbered from 0 (normally the first point is numbered from 1).
    ! The axes of the cube are given by the plot grid axes.
      intent(in) :: self
      integer(kind=kind(1)), intent(in) :: f,l,x,y,z
      real(kind=kind(1.0d0)), dimension(3,f:l,f:l,f:l) :: p
      real(kind=kind(1.0d0)) :: del
      real(kind=kind(1.0d0)) :: x1,x2,x3,y1,y2,y3,z1,z2,z3,ox,oy,oz
      integer(kind=kind(1)) :: n,ix,iy,iz

      n = l - f + 1
      x1 = self%del*self%x_axis(1); x2 = self%del*self%x_axis(2); x3 = self%del*self%x_axis(3)
      y1 = self%del*self%y_axis(1); y2 = self%del*self%y_axis(2); y3 = self%del*self%y_axis(3)
      z1 = self%del*self%z_axis(1); z2 = self%del*self%z_axis(2); z3 = self%del*self%z_axis(3)
      ix = x; iy = y; iz = z
      ox = self%origin(1) + ix*x1 + iy*y1 + iz*z1  ! cube origin
      oy = self%origin(2) + ix*x2 + iy*y2 + iz*z2
      oz = self%origin(3) + ix*x3 + iy*y3 + iz*z3
      x1 = del*self%x_axis(1); x2 = del*self%x_axis(2); x3 = del*self%x_axis(3)  !
      y1 = del*self%y_axis(1); y2 = del*self%y_axis(2); y3 = del*self%y_axis(3)
      z1 = del*self%z_axis(1); z2 = del*self%z_axis(2); z3 = del*self%z_axis(3)
      do iz = f,l
      do iy = f,l
      do ix = f,l
         p(1,ix,iy,iz) = ox + ix*x1 + iy*y1 + iz*z1
         p(2,ix,iy,iz) = oy + ix*x2 + iy*y2 + iz*z2
         p(3,ix,iy,iz) = oz + ix*x3 + iy*y3 + iz*z3
      end do
      end do
      end do

   end subroutine

   subroutine put(self,output)
    type(plotgrid_type) :: self
    ! Put the grid data to file "output"
      type(textfile_type), target, optional :: output
      type(textfile_type), pointer :: out

      if (present(output)) then
        out => output
      else
        out => stdout
      end if
      call flush_(out)
      call text_(out,"PLOTGRID output")
      call flush_(out)
      call show_(out,"Plot grid kind          =", self%plot_kind)
      if (self%orbital /= 0) &
      call show_(out,"Plot orbital no.        =", self%orbital)
      call show_(out,"Number of X grid points =",self%n_x)
      call show_(out,"Number of Y grid points =",self%n_y)
      call show_(out,"Number of Z grid points =",self%n_z)
      call show_(out,"Total number of points  =",self%n_pt)
      call flush_(out)
      call show_(out,"X-axis width            =",self%width(1))
      call show_(out,"Y-axis width            =",self%width(2))
      call show_(out,"Z-axis width            =",self%width(3))
      call show_(out,"Centre point            =",self%centre(1), self%centre(2), self%centre(3))
      call show_(out,"Left hand corner        =",self%origin(1), self%origin(2), self%origin(3))
      call show_(out,"X-axis vector           =",self%x_axis(1), self%x_axis(2), self%x_axis(3))
      call show_(out,"Y-axis vector           =",self%y_axis(1), self%y_axis(2), self%y_axis(3))
      call show_(out,"Z-axis vector           =",self%z_axis(1), self%z_axis(2), self%z_axis(3))
      call show_(out,"Offset vector           =",self%offset(1), self%offset(2), self%offset(3))
      call show_(out,"Box center              =",self%box_centre(1),self%box_centre(2),self%box_centre(3))
      call show_(out,"Bounding box            =",self%bounding_box(1),self%bounding_box(2),self%bounding_box(3))
      call flush_(out)
      call show_(out,"center_atom             =",self%centre_atom)
      call show_(out,"x_atom_1                =",self%x_atom_1)
      call show_(out,"x_atom_2                =",self%x_atom_2)
      call show_(out,"y_atom_1                =",self%y_atom_1)
      call show_(out,"y_atom_2                =",self%y_atom_2)
      call show_(out,"del                     =",self%del)

   end subroutine

end
