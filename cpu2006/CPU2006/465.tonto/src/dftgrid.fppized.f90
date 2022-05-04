!-------------------------------------------------------------------------------
!
! DFTGRID: Numerical integration grid for DFT calculations
!
! This routine gets a 3-D grid which is a combination of a
! radial grid and a spherical grid.
!
! In getting a complete molecular grid array, firstly a grid at the
! origin is made. This is the purpose of the routine "make_atom_grid".
! This grid is then effectively copied to each atom of the molecule.
! More exactly, for each atom in the molecule, this grid at the origin
! is copied, re-scaled depending on the Bragg-Slater radii,
! and then displaced so that its origin is now centred on the atom.
!
! Each re-scaled, displaced grid is then "partitioned".  The partitioning
! modifies the weights of the displaced grid so that effectively it doesn't
! overlap with the displaced grids on all the other atoms.
!
! The displace and partition of the original "atom" grid is done by
! the routine "rescale_displace_partition".
!
! Copyright (C) S. K. Wolff, 1999
!     hacked by dylan, 10 minutes later.
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
! $Id: dftgrid.foo,v 1.18.2.11 2004/04/21 07:28:57 reaper Exp $
!
!-------------------------------------------------------------------------------

module DFTGRID_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: equals_
   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: create_copy_
   use REALVEC_MODULE, only: destroy_

   use INT_MODULE, only: to_str_

   use PARALLEL_MODULE, only: sum_vectors_
   use PARALLEL_MODULE, only: n_proc_
   use PARALLEL_MODULE, only: this_proc_

   use ATOM_MODULE, only: chemical_symbol_
   use ATOM_MODULE, only: bragg_slater_radius_

   use GAUSSIAN_MODULE, only: make_grid_
   use GAUSSIAN_MODULE, only: n_comp_

   use REALMAT3_MODULE, only: create_
   use REALMAT3_MODULE, only: destroy_

   use SHELL1_MODULE, only: make_grid_
   use SHELL1_MODULE, only: make_nabla_grid_

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

   use STR_MODULE, only: to_lower_case_

   use ATOMVEC_MODULE, only: nullify_ptr_part_
   use ATOMVEC_MODULE, only: atom_index_from_pos_
   use ATOMVEC_MODULE, only: create_
   use ATOMVEC_MODULE, only: destroy_

   use REAL_MODULE, only: arcsinh_

   use ARCHIVE_MODULE, only: set_genre_
   use ARCHIVE_MODULE, only: set_root_name_
   use ARCHIVE_MODULE, only: set_
   use ARCHIVE_MODULE, only: set_name_
   use ARCHIVE_MODULE, only: nullify_ptr_part_
   use ARCHIVE_MODULE, only: exists_
   use ARCHIVE_MODULE, only: write_
   use ARCHIVE_MODULE, only: read_
   use ARCHIVE_MODULE, only: copy_

   use REALMAT_MODULE, only: create_
   use REALMAT_MODULE, only: create_copy_
   use REALMAT_MODULE, only: destroy_

   use CPXVEC_MODULE, only: create_
   use CPXVEC_MODULE, only: destroy_
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

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   private    make_atom_grid_
   interface make_atom_grid_
      module procedure make_atom_grid
   end interface

   public    integrate_
   interface integrate_
      module procedure integrate
   end interface

   public    read_spherical_grid_kind_
   interface read_spherical_grid_kind_
      module procedure read_spherical_grid_kind
   end interface

   public    read_keywords_
   interface read_keywords_
      module procedure read_keywords
   end interface

   private    make_euler_maclaurin_grid_
   interface make_euler_maclaurin_grid_
      module procedure make_euler_maclaurin_grid
   end interface

   public    read_euler_maclaurin_alpha_
   interface read_euler_maclaurin_alpha_
      module procedure read_euler_maclaurin_alpha
   end interface

   public    r_gill96_exchange_functional_
   interface r_gill96_exchange_functional_
      module procedure r_gill96_exchange_functional
   end interface

   public    read_gauss_chebyshev_m_
   interface read_gauss_chebyshev_m_
      module procedure read_gauss_chebyshev_m
   end interface

   public    r_lyp_correlation_functional_
   interface r_lyp_correlation_functional_
      module procedure r_lyp_correlation_functional
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    make_NL_matrix_elements_of_
   interface make_NL_matrix_elements_of_
      module procedure make_NL_matrix_elements_of
   end interface

   public    u_lda_exchange_functional_
   interface u_lda_exchange_functional_
      module procedure u_lda_exchange_functional
   end interface

   public    u_gill96_exchange_functional_
   interface u_gill96_exchange_functional_
      module procedure u_gill96_exchange_functional
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    u_lyp_correlation_functional_
   interface u_lyp_correlation_functional_
      module procedure u_lyp_correlation_functional
   end interface

   public    set_high_accuracy_
   interface set_high_accuracy_
      module procedure set_high_accuracy
   end interface

   public    integrate_molecular_property_
   interface integrate_molecular_property_
      module procedure integrate_molecular_property
   end interface

   public    r_becke88_exchange_functional_
   interface r_becke88_exchange_functional_
      module procedure r_becke88_exchange_functional
   end interface

   public    d_u_b88_exchange_functional_
   interface d_u_b88_exchange_functional_
      module procedure d_u_b88_exchange_functional
   end interface

   private    pm_perm_point_11_
   interface pm_perm_point_11_
      module procedure pm_perm_point_11
   end interface

   private    pm_perm_point_12_
   interface pm_perm_point_12_
      module procedure pm_perm_point_12
   end interface

   public    read_units_
   interface read_units_
      module procedure read_units
   end interface

   private    pm_perm_point_13_
   interface pm_perm_point_13_
      module procedure pm_perm_point_13
   end interface

   public    read_radial_grid_order_
   interface read_radial_grid_order_
      module procedure read_radial_grid_order
   end interface

   public    genre_
   interface genre_
      module procedure genre
   end interface

   public    set_lebedev_data_
   interface set_lebedev_data_
      module procedure set_lebedev_data
   end interface

   public    make_cmplx_matrix_elements_of_
   interface make_cmplx_matrix_elements_of_
      module procedure make_cmplx_matrix_elements_of
   end interface

   public    d_u_lda_exchange_functional_
   interface d_u_lda_exchange_functional_
      module procedure d_u_lda_exchange_functional
   end interface

   public    read_gauss_chebyshev_alpha_
   interface read_gauss_chebyshev_alpha_
      module procedure read_gauss_chebyshev_alpha
   end interface

   public    u_becke88_exchange_functional_
   interface u_becke88_exchange_functional_
      module procedure u_becke88_exchange_functional
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   public    make_SO_matrix_elements_of_
   interface make_SO_matrix_elements_of_
      module procedure make_SO_matrix_elements_of
   end interface

   public    set_grid_data_
   interface set_grid_data_
      module procedure set_grid_data
   end interface

   public    get_grid_
   interface get_grid_
      module procedure get_grid
   end interface

   public    set_radial_grid_data_
   interface set_radial_grid_data_
      module procedure set_radial_grid_data
   end interface

   public    read_euler_maclaurin_m_
   interface read_euler_maclaurin_m_
      module procedure read_euler_maclaurin_m
   end interface

   public    read_radial_grid_kind_
   interface read_radial_grid_kind_
      module procedure read_radial_grid_kind
   end interface

   public    set_spherical_grid_data_
   interface set_spherical_grid_data_
      module procedure set_spherical_grid_data
   end interface

   private    make_gauss_chebyshev_grid_
   interface make_gauss_chebyshev_grid_
      module procedure make_gauss_chebyshev_grid
   end interface

   public    process_keyword_
   interface process_keyword_
      module procedure process_keyword
   end interface

   private    pm_perm_point_21_
   interface pm_perm_point_21_
      module procedure pm_perm_point_21
   end interface

   public    put_
   interface put_
      module procedure put
   end interface

   private    pm_perm_point_22_
   interface pm_perm_point_22_
      module procedure pm_perm_point_22
   end interface

   public    d_r_gill96_exchange_functional_
   interface d_r_gill96_exchange_functional_
      module procedure d_r_gill96_exchange_functional
   end interface

   public    read_spherical_grid_order_
   interface read_spherical_grid_order_
      module procedure read_spherical_grid_order
   end interface

   public    d_r_lyp_correlation_functional_
   interface d_r_lyp_correlation_functional_
      module procedure d_r_lyp_correlation_functional
   end interface

   public    r_lda_exchange_functional_
   interface r_lda_exchange_functional_
      module procedure r_lda_exchange_functional
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   private    make_spherical_grid_
   interface make_spherical_grid_
      module procedure make_spherical_grid
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    make_matrix_elements_of_
   interface make_matrix_elements_of_
      module procedure make_matrix_elements_of
      module procedure make_matrix_elements_of_1
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    read_becke_m_partition_power_
   interface read_becke_m_partition_power_
      module procedure read_becke_m_partition_power
   end interface

   public    make_grid_
   interface make_grid_
      module procedure make_grid
   end interface

   public    d_r_b88_exchange_functional_
   interface d_r_b88_exchange_functional_
      module procedure d_r_b88_exchange_functional
   end interface

   public    d_u_gill96_exchange_functional_
   interface d_u_gill96_exchange_functional_
      module procedure d_u_gill96_exchange_functional
   end interface

   public    read_junk_
   interface read_junk_
      module procedure read_junk
   end interface

   private    rescale_displace_partition_
   interface rescale_displace_partition_
      module procedure rescale_displace_partition
      module procedure rescale_displace_partition_1
   end interface

   private    make_radial_grid_
   interface make_radial_grid_
      module procedure make_radial_grid
   end interface

   private    make_lebedev_grid_
   interface make_lebedev_grid_
      module procedure make_lebedev_grid
   end interface

   public    d_u_lyp_correlation_functional_
   interface d_u_lyp_correlation_functional_
      module procedure d_u_lyp_correlation_functional
   end interface

   private    get_atom_grid_
   interface get_atom_grid_
      module procedure get_atom_grid
   end interface

   public    d_r_lda_exchange_functional_
   interface d_r_lda_exchange_functional_
      module procedure d_r_lda_exchange_functional
   end interface

   private    pm_perm_point_31_
   interface pm_perm_point_31_
      module procedure pm_perm_point_31
   end interface

contains

!  **************************
!  Create and destroy methods
!  **************************

   subroutine create(self,root_name,name,genre,format)
    type(dftgrid_type) :: self
    ! Create an object
      pointer :: self
      character(128), optional :: root_name,name,genre,format

      nullify(self)
      allocate(self)

      call nullify_ptr_part_(self)
      call set_defaults_(self)
      call set_(self%archive,root_name,name,genre,format)

   end subroutine

   subroutine destroy(self)
    type(dftgrid_type) :: self
    ! Destroy object
      pointer :: self

      if (.not. associated(self)) then;   return; end if
      call destroy_ptr_part_(self)

      deallocate(self)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(dftgrid_type) :: self

     if (associated(self%single_atom_points)) call destroy_(self%single_atom_points)
     if (associated(self%single_atom_weights)) call destroy_(self%single_atom_weights)

   end subroutine

   subroutine nullify_ptr_part(self)
    type(dftgrid_type) :: self

     nullify(self%single_atom_points)
     nullify(self%single_atom_weights)

   end subroutine

!   created result(res)
!   ! Returns true if self has been created
!      self :: pointer
!      res :: logical(kind=kind(.true.))
!      res = associated(self)
!   end

!   destroyed result(res)
!   ! Return .true. if object is destroyed
!      self :: pointer
!      res :: logical(kind=kind(.true.))
!      res = .not. associated(self)
!   end

   subroutine create_copy(self,d)
    type(dftgrid_type) :: self
    ! Create a copy a dftgrid "d"
      pointer :: self
       type(dftgrid_type) :: d

      call create_(self)
      call nullify_ptr_part_(self)
      call copy_(self,d)

   end subroutine

   subroutine copy(self,d)
    type(dftgrid_type) :: self
    ! Copy a dftgrid "d"
      type(dftgrid_type) :: d

      if (associated(d%single_atom_points)) call create_copy_(self%single_atom_points,d%single_atom_points)
      if (associated(d%single_atom_weights)) call create_copy_(self%single_atom_weights,d%single_atom_weights)
      self = d
      call copy_(self%archive,d%archive)

   end subroutine

   subroutine set_defaults(self)
    type(dftgrid_type) :: self
    ! Set up a default dftgrid object

      self%spherical_grid_kind     = "lebedev"
      self%spherical_grid_order    = 35
      self%radial_grid_kind        = "gauss-chebyshev"
      self%radial_grid_order       = 60
      self%becke_m_partition_power = 2.0d0
      self%gauss_chebyshev_m       = 1.0d0
      self%gauss_chebyshev_alpha   = 3.0d0
      self%euler_maclaurin_m       = 2.0d0
      self%euler_maclaurin_alpha   = 2.0d0
      call nullify_ptr_part_(self%archive)
      call set_root_name_(self%archive,"dftgrid")
      call set_name_(self%archive,"grid")
      call set_genre_(self%archive,genre_(self))
      call set_grid_data_(self)

   end subroutine

   subroutine set_high_accuracy(self)
    type(dftgrid_type) :: self
    ! Set up a default dftgrid object

      self%spherical_grid_kind = 'lebedev'
      self%spherical_grid_order = 53
      self%radial_grid_kind = 'gauss-chebyshev'
      self%radial_grid_order = 250
      self%becke_m_partition_power = 2.0d0
      self%gauss_chebyshev_m       = 1.0d0
      self%gauss_chebyshev_alpha   = 3.0d0
      self%euler_maclaurin_m       = 2.0d0
      self%euler_maclaurin_alpha   = 2.0d0
      call set_root_name_(self%archive,"dftgrid")
      call set_name_(self%archive,"grid")
      call set_genre_(self%archive,genre_(self))
      call set_grid_data_(self)

   end subroutine

   function genre(self) result(a_kind)
    type(dftgrid_type) :: self
    ! Return the archive "kind" using the current dftgrid settings
      character(128) :: a_kind

      a_kind =                 trim(self%spherical_grid_kind) // "_"
      a_kind = trim(a_kind) // trim(to_str_(self%spherical_grid_order)) // "_"
      a_kind = trim(a_kind) // trim(self%radial_grid_kind) // "_"
      a_kind = trim(a_kind) // trim(to_str_(self%radial_grid_order))

   end function

!  *************
!  Input methods
!  *************

   recursive subroutine read_keywords(self)
    type(dftgrid_type) :: self
    ! Read data from "stdin" using keyword style input.
    ! The following code is inherited from OBJECT
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","DFTGRID:read_keywords ... expecting open bracket symbol, {")
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
    type(dftgrid_type) :: self
    ! Process command "keyword". Any required data needed by the "keyword" is
    ! inputted from "stdin".
      character(*) :: keyword
      character(128) :: word

      word = keyword
      call to_lower_case_(word)
      self%finalized = .false.
      select case (word)
         case ("}                       ")   ! exit case
         case ("becke_m_partition_power="); call read_becke_m_partition_power_(self)
         case ("euler_maclaurin_alpha=  "); call read_euler_maclaurin_alpha_(self)
         case ("euler_maclaurin_m=      "); call read_euler_maclaurin_m_(self)
         case ("gauss_chebyshev_alpha=  "); call read_gauss_chebyshev_alpha_(self)
         case ("gauss_chebyshev_m=      "); call read_gauss_chebyshev_m_(self)
         case ("put                     "); call put_(self)
         case ("radial_grid_kind=       "); call read_radial_grid_kind_(self)
         case ("radial_grid_order=      "); call read_radial_grid_order_(self)
         case ("set_defaults            "); call set_defaults_(self)
         case ("set_high_accuracy       "); call set_high_accuracy_(self)
         case ("spherical_grid_kind=    "); call read_spherical_grid_kind_(self)
         case ("spherical_grid_order=   "); call read_spherical_grid_order_(self)
         case default;                    allocate(tonto%known_keywords(13))
         tonto%known_keywords(1) = "}                       "
         tonto%known_keywords(2) = "becke_m_partition_power="
         tonto%known_keywords(3) = "euler_maclaurin_alpha=  "
         tonto%known_keywords(4) = "euler_maclaurin_m=      "
         tonto%known_keywords(5) = "gauss_chebyshev_alpha=  "
         tonto%known_keywords(6) = "gauss_chebyshev_m=      "
         tonto%known_keywords(7) = "put                     "
         tonto%known_keywords(8) = "radial_grid_kind=       "
         tonto%known_keywords(9) = "radial_grid_order=      "
         tonto%known_keywords(10) = "set_defaults            "
         tonto%known_keywords(11) = "set_high_accuracy       "
         tonto%known_keywords(12) = "spherical_grid_kind=    "
         tonto%known_keywords(13) = "spherical_grid_order=   "
         call unknown_(tonto,word,"DFTGRID:process_keyword")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_units(self)
    type(dftgrid_type) :: self
    ! Read a string which describes the units to be used
    ! The following code is inherited from OBJECT

      call set_default_units_(stdin,next_str_(stdin))

   end subroutine

   subroutine read_junk(self)
    type(dftgrid_type) :: self
    ! Read in a junk string, useful for ignoring a field
    ! The following code is inherited from OBJECT

      call skip_next_item_(stdin)

   end subroutine

   subroutine read_spherical_grid_kind(self)
    type(dftgrid_type) :: self
    ! Read in the spherical grid kind

      call read_(stdin,self%spherical_grid_kind)
      select case (self%spherical_grid_kind)
         case("lebedev")
         case default;  allocate(tonto%known_keywords(1))
         tonto%known_keywords(1) = "lebedev"
         call unknown_(tonto,self%spherical_grid_kind,"DFTGRID:read_spherical_grid_kind")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_spherical_grid_order(self)
    type(dftgrid_type) :: self
    ! Read in the spherical grid order
      character(128) :: word

      call read_(stdin,self%spherical_grid_order)
      word = to_str_(self%spherical_grid_order)
      select case (self%spherical_grid_order)
         case (9)
         case (11)
         case (13)
         case (15)
         case (17)
         case (19)
         case (23)
         case (25)
         case (27)
         case (29)
         case (35)
         case (41)
         case (47)
         case (53)
         case (59)
         case default;  allocate(tonto%known_keywords(0))
         call unknown_(tonto,word,"DFTGRID:read_spherical_grid_order")
         deallocate(tonto%known_keywords)
      end select
      call set_grid_data_(self)

   end subroutine

   subroutine read_radial_grid_kind(self)
    type(dftgrid_type) :: self
    ! Read in the radial grid kind

      call read_(stdin,self%radial_grid_kind)
      select case (self%radial_grid_kind)
         case("gauss-chebychev")
         case("euler-maclaurin")
         case default;  allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "gauss-chebychev"
         tonto%known_keywords(2) = "euler-maclaurin"
         call unknown_(tonto,self%radial_grid_kind,"DFTGRID:read_radial_grid_kind")
         deallocate(tonto%known_keywords)
      end select
      call set_grid_data_(self)

   end subroutine

   subroutine read_radial_grid_order(self)
    type(dftgrid_type) :: self
    ! Read in the radial grid order

      call read_(stdin,self%radial_grid_order)
      call die_if_(tonto,self%radial_grid_order<=0,"DFTGRID:read_radial_grid_order ... must be positive")
      call set_grid_data_(self)

   end subroutine

   subroutine read_becke_m_partition_power(self)
    type(dftgrid_type) :: self
    ! Read the Becke "m" partition power. Refer to paper.

      call read_(stdin,self%becke_m_partition_power)
      call die_if_(tonto,self%becke_m_partition_power<=0,"DFTGRID:read_becke_m_partition_power ... must be positive")
      call set_grid_data_(self)

   end subroutine

   subroutine read_gauss_chebyshev_m(self)
    type(dftgrid_type) :: self
    ! Read the Gauss-Chebyshev "m" partition power. Refer to paper.

      call read_(stdin,self%gauss_chebyshev_m)
      call die_if_(tonto,self%gauss_chebyshev_m<=0,"DFTGRID:read_gauss_chebyshev_m ... must be positive")
      call set_grid_data_(self)

   end subroutine

   subroutine read_gauss_chebyshev_alpha(self)
    type(dftgrid_type) :: self
    ! Read the Gauss-Chebyshev "alpha" partition power. Refer to paper.

      call read_(stdin,self%gauss_chebyshev_alpha)
      call die_if_(tonto,self%gauss_chebyshev_alpha<=0,"DFTGRID:read_gauss_chebyshev_alpha ... must be positive")
      call set_grid_data_(self)

   end subroutine

   subroutine read_euler_maclaurin_m(self)
    type(dftgrid_type) :: self
    ! Read the Euler Maclaurin "m" value. Refer to paper.

      call read_(stdin,self%euler_maclaurin_m)
      call die_if_(tonto,self%euler_maclaurin_m<=0,"DFTGRID:read_euler_maclaurin_m ... must be positive")
      call set_grid_data_(self)

   end subroutine

   subroutine read_euler_maclaurin_alpha(self)
    type(dftgrid_type) :: self
    ! Read the Euler Maclaurin "alpha" value. Refer to paper.

      call read_(stdin,self%euler_maclaurin_alpha)
      call die_if_(tonto,self%euler_maclaurin_alpha<=0,"DFTGRID:read_euler_maclaurin_alpha ... must be positive")
      call set_grid_data_(self)

   end subroutine

!  *****************************************************************
!  Routines which evaulate the number of grid points for each scheme
!  *****************************************************************

   subroutine set_grid_data(self)
    type(dftgrid_type) :: self
    ! Set all the grid data preliminary to grid generation

      call set_radial_grid_data_(self)
      call set_spherical_grid_data_(self)
      self%n_pts = self%n_spherical_pts*self%n_radial_pts
      call set_genre_(self%archive,trim(genre_(self)))
      self%finalized = .true.
      if (associated(self%single_atom_points)) call destroy_(self%single_atom_points)
      if (associated(self%single_atom_weights)) call destroy_(self%single_atom_weights)

   end subroutine

   subroutine set_radial_grid_data(self)
    type(dftgrid_type) :: self
    ! Set the radial grid data

      select case (self%radial_grid_kind)
         case ("gauss-chebyshev");   self%n_radial_pts = self%radial_grid_order
         case ("gauss_chebyshev");   self%n_radial_pts = self%radial_grid_order
         case ("euler-maclaurin");   self%n_radial_pts = self%radial_grid_order
         case ("euler_maclaurin");   self%n_radial_pts = self%radial_grid_order
         case default;               allocate(tonto%known_keywords(4))
         tonto%known_keywords(1) = "gauss-chebyshev"
         tonto%known_keywords(2) = "gauss_chebyshev"
         tonto%known_keywords(3) = "euler-maclaurin"
         tonto%known_keywords(4) = "euler_maclaurin"
         call unknown_(tonto,self%radial_grid_kind,"DFTGRID:set_radial_grid_data")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine set_spherical_grid_data(self)
    type(dftgrid_type) :: self
    ! Set any spherical grid data parameters. At the moment
    ! only spherical Lebedev grids are available

      select case (self%spherical_grid_kind)
         case ("lebedev");  call set_lebedev_data_(self)
         case default;      allocate(tonto%known_keywords(1))
         tonto%known_keywords(1) = "lebedev"
         call unknown_(tonto,self%spherical_grid_kind,"DFTGRID:set_spherical_grid_data")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine set_lebedev_data(self,mm1,mm2,mm3,nn1,nn2,nn3)
    type(dftgrid_type) :: self
    ! Sets the Lebedev grid parameters
    ! NOTE: This should match "get_lebedev_wxyz"
      integer(kind=kind(1)), intent(out), optional :: mm1, mm2, mm3, nn1, nn2, nn3
      integer(kind=kind(1)) :: m1, m2, m3, n1, n2, n3
       ! NOTE: If ai = 0.0d0, then .mi  = 0, for i = 1, 2, 3.

      m1 = 6; m2 = 12; m3 = 8
      select case (self%spherical_grid_order)
         case (9);  m2 = 0;         n1 =  0; n2 =  1; n3 =  0
         case (11);                 n1 =  1; n2 =  0; n3 =  0
         case (13); m2 = 0; m3 = 0; n1 =  2; n2 =  1; n3 =  0
         case (15); m2 = 0;         n1 =  2; n2 =  1; n3 =  0
         case (17); m1 = 0;         n1 =  3; n2 =  1; n3 =  0
         case (19);                 n1 =  3; n2 =  0; n3 =  1
         case (23);                 n1 =  4; n2 =  1; n3 =  1
         case (25); m2 = 0;         n1 =  5; n2 =  2; n3 =  1
         case (27);                 n1 =  5; n2 =  1; n3 =  2
         case (29); m2 = 0;         n1 =  6; n2 =  2; n3 =  2
         case (35);                 n1 =  7; n2 =  2; n3 =  4
         case (41); m2 = 0;         n1 =  9; n2 =  3; n3 =  6
         case (47);                 n1 = 10; n2 =  3; n3 =  9
         case (53); m2 = 0;         n1 = 12; n2 =  4; n3 = 12
         case (59);                 n1 = 13; n2 =  4; n3 = 16
         case default; call die_(tonto,"DFTGRID:set_lebedev_data ... Lebedev grid order number doesn't exist")
      end select
      self%n_spherical_pts = m1 + m2 + m3 + 24*n1 + 24*n2 + 48*n3
      if (present(mm1)) mm1 = m1
      if (present(mm2)) mm2 = m2
      if (present(mm3)) mm3 = m3
      if (present(nn1)) nn1 = n1
      if (present(nn2)) nn2 = n2
      if (present(nn3)) nn3 = n3

   end subroutine

!  ************************
!  Grid integration methods
!  ************************

   function integrate(self,f,atom) result(res)
    type(dftgrid_type) :: self
    ! Integrate the vector function "f" over grids defined for the atoms
    ! in the ATOMVEC "atom".
      interface
         function f(r) result(res)
             real(kind=kind(1.0d0)), dimension(3) :: r
            real(kind=kind(1.0d0)) :: res
         end function
      end interface
      type(atom_type), dimension(:) :: atom
      real(kind=kind(1.0d0)) :: res
      integer(kind=kind(1)) :: n_pt, a, n
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt
      real(kind=kind(1.0d0)), dimension(:), pointer :: wt

      call ensure_(tonto,self%finalized,"DFTGRID:integrate ... call the set_grid_data routine")
      res = 0.0d0
      n_pt = self%n_pts
      call create_(pt,n_pt,3);  call create_(wt,n_pt)
      call get_atom_grid_(self)
      do a = 1, size(atom)
         call rescale_displace_partition_(self,pt,wt,a,atom)
         do n = 1,n_pt
            res = res + wt(n)*f(pt(n,:))
         end do
      end do
      call destroy_(wt);  call destroy_(pt)

   end function

   subroutine make_matrix_elements_of(self,V,g,ans)
    type(dftgrid_type) :: self
    ! Integrate the matrix elements of a multiplicative operator "V"
    ! between all components of two gaussian functions given by the
    ! gaussian-pair "g". The result of the integration is matrix "ans".
    ! Operator "V" is represented by a function which returns a "v_grid"
    ! on a set of "pts".
      interface
         subroutine V(v_grid,pts)
            real(kind=kind(1.0d0)), dimension(:) :: v_grid
            real(kind=kind(1.0d0)), dimension(:,:) :: pts
         end subroutine
      end interface
      type(gaussian2_type) :: g
      real(kind=kind(1.0d0)), dimension(:,:) :: ans
      integer(kind=kind(1)) :: n_pt,n,a,b
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt,pos,a_grid,b_grid
      real(kind=kind(1.0d0)), dimension(:), pointer :: wt,scale,v_grid

      call ensure_(tonto,self%finalized,"DFTGRID:make_matrix_elements_of ... call the set_grid_data routine")
      call ensure_(tonto,size(ans,1)==n_comp_(g%a),"DFTGRID:make_matrix_elements_of ... wrong size, ans")
      call ensure_(tonto,size(ans,2)==n_comp_(g%b),"DFTGRID:make_matrix_elements_of ... wrong size, ans")
      ans = 0.0d0
      n_pt = self%n_pts
      call create_(pt,n_pt,3);  call create_(wt,n_pt)
      call create_(v_grid,n_pt)
      call create_(a_grid,n_pt,n_comp_(g%a))
      call create_(b_grid,n_pt,n_comp_(g%b))
      call get_atom_grid_(self)
      if (equals_(g%a%pos,g%b%pos)) then
         call create_(pos,1,3); pos(1,:) = g%a%pos
         call create_(scale,1); scale(:) = 1.0d0
      else
         call create_(pos,2,3); pos(1,:) = g%a%pos
                          pos(2,:) = g%b%pos
         call create_(scale,2); scale(:) = 1.0d0
      end if
      do n = 1,size(pos,1)
         call rescale_displace_partition_(self,pt,wt,n,pos,scale)
         call V(v_grid,pt)
         v_grid = wt*v_grid
         call make_grid_(g%a,a_grid,pt)
         call make_grid_(g%b,b_grid,pt)
         do a = 1,n_comp_(g%a)
         do b = 1,n_comp_(g%b)
            wt = a_grid(:,a)*b_grid(:,b)
            ans(a,b) = ans(a,b) + sum(wt*v_grid)
         end do
         end do
      end do
      call destroy_(scale)
      call destroy_(pos)
      call destroy_(b_grid); call destroy_(a_grid)
      call destroy_(v_grid)
      call destroy_(wt);  call destroy_(pt)

   end subroutine

   subroutine make_cmplx_matrix_elements_of(self,V,g,ans)
    type(dftgrid_type) :: self
    ! Integrate the matrix elements of a complex multiplicative operator "V"
    ! between all components of two real gaussian functions given by the
    ! gaussian-pair "g". The result of the integration is matrix "ans".
    ! Operator "V" is represented by a function which returns a "v_grid"
    ! on a set of "pts".
      interface
         subroutine V(v_grid,pts)
            complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: v_grid
            real(kind=kind(1.0d0)), dimension(:,:) :: pts
         end subroutine
      end interface
      type(gaussian2_type) :: g
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: ans
      integer(kind=kind(1)) :: n_pt,n,a,b
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt,pos,a_grid,b_grid
      real(kind=kind(1.0d0)), dimension(:), pointer :: wt,scale
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: v_grid

      call ensure_(tonto,self%finalized,"DFTGRID:make_cmplx_matrix_elements_of ... call the set_grid_data routine")
      call ensure_(tonto,size(ans,1)==n_comp_(g%a),"DFTGRID:make_cmplx_matrix_elements_of ... wrong size, ans")
      call ensure_(tonto,size(ans,2)==n_comp_(g%b),"DFTGRID:make_cmplx_matrix_elements_of ... wrong size, ans")
      ans = 0.0d0
      n_pt = self%n_pts
      call create_(pt,n_pt,3);  call create_(wt,n_pt)
      call create_(v_grid,n_pt)
      call create_(a_grid,n_pt,n_comp_(g%a))
      call create_(b_grid,n_pt,n_comp_(g%b))
      call get_atom_grid_(self)
      if (equals_(g%a%pos,g%b%pos)) then
         call create_(pos,1,3); pos(1,:) = g%a%pos
         call create_(scale,1); scale(:) = 1.0d0
      else
         call create_(pos,2,3); pos(1,:) = g%a%pos
                          pos(2,:) = g%b%pos
         call create_(scale,2); scale(:) = 1.0d0
      end if
      do n = 1,size(pos,1)
         call rescale_displace_partition_(self,pt,wt,n,pos,scale)
         call V(v_grid,pt)
         v_grid = wt*v_grid
         call make_grid_(g%a,a_grid,pt)
         call make_grid_(g%b,b_grid,pt)
         do a = 1,n_comp_(g%a)
         do b = 1,n_comp_(g%b)
            wt = a_grid(:,a)*b_grid(:,b)
            ans(a,b) = ans(a,b) + sum(wt*v_grid)
         end do
         end do
      end do
      call destroy_(scale)
      call destroy_(pos)
      call destroy_(b_grid); call destroy_(a_grid)
      call destroy_(v_grid)
      call destroy_(wt);  call destroy_(pt)

   end subroutine

   function integrate_molecular_property(self,X,mol) result(res)
    type(dftgrid_type) :: self
    ! Integrate a scalar molecular property, which is represented by a
    ! subroutine "X" which returns "values" of the property in a vector,
    ! given "mol" as the molecule, and "pts" as a set of points.
    ! The result of the integration is "res".
      interface
         subroutine X(mol,values,pts)
   use TYPES_MODULE
            type(mol_type) :: mol
            real(kind=kind(1.0d0)), dimension(:) :: values
            real(kind=kind(1.0d0)), dimension(:,:) :: pts
         end subroutine
      end interface
      type(mol_type) :: mol
      real(kind=kind(1.0d0)) :: res
      integer(kind=kind(1)) :: n_pt, a, n
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt
      real(kind=kind(1.0d0)), dimension(:), pointer :: wt,values

      call ensure_(tonto,self%finalized,"DFTGRID:integrate_molecular_property ... call the set_grid_data routine")
      res = 0.0d0
      n_pt = self%n_pts
      call create_(pt,n_pt,3);  call create_(wt,n_pt)
      call create_(values,n_pt)
      call get_atom_grid_(self)
      do a = 1, mol%n_atom
         call rescale_displace_partition_(self,pt,wt,a, mol%atom)
         call X(mol,values,pt)
         do n = 1,n_pt
            res = res+ wt(n)*values(n)
         end do
      end do
      call destroy_(values)
      call destroy_(wt);  call destroy_(pt)

   end function

   subroutine make_matrix_elements_of_1(self,V,mol,shell,ans)
    type(dftgrid_type) :: self
    ! Integrate the matrix elements of a multiplicative operator "V" for
    ! molecule "mol" between all components of two A.O. shells given by
    ! the shell-pair "shell". The result of the integration is matrix "ans".
    ! Operator "V" is represented by a subroutine which returns "v_grid"
    ! on a set of "pts" for a given molecule "mol".
    !
    ! This routine calculates: (a|V|b)
    !
      interface
         subroutine V(mol,v_grid,pts)
   use TYPES_MODULE
            type(mol_type) :: mol
            real(kind=kind(1.0d0)), dimension(:) :: v_grid
            real(kind=kind(1.0d0)), dimension(:,:) :: pts
         end subroutine
      end interface
      type(mol_type) :: mol
      type(shell2_type) :: shell
      real(kind=kind(1.0d0)), dimension(:,:) :: ans
      type(atom_type), dimension(:), pointer :: atom1
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt,a_grid,b_grid
      real(kind=kind(1.0d0)), dimension(:), pointer :: wt,v_grid
      integer(kind=kind(1)) :: n_pt, n,a,b,atom_a,atom_b

      call ensure_(tonto,self%finalized,"DFTGRID:make_matrix_elements_of_1 ... call the set_grid_data routine")
      ans = 0.0d0
      n_pt = self%n_pts
      call create_(pt,n_pt,3);  call create_(wt,n_pt)
      call create_(v_grid,n_pt)
      call create_(a_grid,n_pt,shell%a%n_comp)
      call create_(b_grid,n_pt,shell%b%n_comp)
      call get_atom_grid_(self)
      atom_a = atom_index_from_pos_(mol%atom,shell%a%pos)
      atom_b = atom_index_from_pos_(mol%atom,shell%b%pos)
      if (atom_a==atom_b) then
         call create_(atom1,1)
         atom1(1) = mol%atom(atom_a)
      else
         call create_(atom1,2)
         atom1(1) = mol%atom(atom_a)
         atom1(2) = mol%atom(atom_b)
      end if
      do n = 1,size(atom1)
         call rescale_displace_partition_(self,pt,wt,n, atom1)
         call V(mol,v_grid,pt)
         v_grid = wt*v_grid
         call make_grid_(shell%a,a_grid,pt)
         call make_grid_(shell%b,b_grid,pt)
         do a = 1,shell%a%n_comp
         do b = 1,shell%b%n_comp
            wt = a_grid(:,a)*b_grid(:,b)
            ans(a,b) = ans(a,b) + sum(wt*v_grid)
         end do
         end do
      end do
      call nullify_ptr_part_(atom1); call destroy_(atom1)
      call destroy_(b_grid); call destroy_(a_grid)
      call destroy_(v_grid)
      call destroy_(wt);  call destroy_(pt)
      call flush_(stdout)

   end subroutine

   subroutine make_NL_matrix_elements_of(self,V,mol,shell,ans)
    type(dftgrid_type) :: self
    ! This is the non-local version of "make_matrix_elements_of".
    ! It is specifically designed to calculate the matrix elements of
    ! non-local functionals --- i.e. that contain rho and nabla_rho.
    ! It calculates:
    !
    !         integer(kind=kind(1))[dF/d(rho)*ab] + integer(kind=kind(1))[dF/d(nabla_rho)*nabla(ab)],
    !
    ! where a, b are basis functions, and integer(kind=kind(1)) = "integral of".
    !
    ! Some of the variables used in this routine are:
    !
    ! ans       = matrix result of calculation
    ! v_grid    = dF/d(rho) on the grid of points
    ! nl_v_grid = dF/d(nabla_rho) = non-local v_grid on the grid of points
    ! a_grid    = basis function values on the grid of points
    ! nabla_a_grid = nabla of basis function values on the grid of points
    !
      interface
         subroutine V(mol,v_grid,nl_v_grid,pts)
   use TYPES_MODULE
            type(mol_type) :: mol
            real(kind=kind(1.0d0)), dimension(:) :: v_grid
            real(kind=kind(1.0d0)), dimension(:,:) :: nl_v_grid
            real(kind=kind(1.0d0)), dimension(:,:) :: pts
         end subroutine
      end interface
      type(mol_type) :: mol
      type(shell2_type) :: shell
      real(kind=kind(1.0d0)), dimension(:,:) :: ans
      type(atom_type), dimension(:), pointer :: atom1
      integer(kind=kind(1)) :: n_pt, n,a,b,atom_a,atom_b
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt
      real(kind=kind(1.0d0)), dimension(:), pointer :: wt,v_grid
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: nl_v_grid
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: a_grid,b_grid
      real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: nabla_a_grid, nabla_b_grid

      call ensure_(tonto,self%finalized,"DFTGRID:make_NL_matrix_elements_of ... call the set_grid_data routine")
      ans = 0.0d0
      n_pt = self%n_pts
      call create_(pt,n_pt,3);  call create_(wt,n_pt)
      call create_(v_grid,n_pt)
      call create_(nl_v_grid,n_pt,3)
      call create_(a_grid,n_pt,shell%a%n_comp)
      call create_(b_grid,n_pt,shell%b%n_comp)
      call create_(nabla_a_grid,n_pt,shell%a%n_comp,3)
      call create_(nabla_b_grid,n_pt,shell%b%n_comp,3)
      call get_atom_grid_(self)
      atom_a = atom_index_from_pos_(mol%atom,shell%a%pos)
      atom_b = atom_index_from_pos_(mol%atom,shell%b%pos)
      if (atom_a==atom_b) then
         call create_(atom1,1)
         atom1(1) = mol%atom(atom_a)
      else
         call create_(atom1,2)
         atom1(1) = mol%atom(atom_a)
         atom1(2) = mol%atom(atom_b)
      end if
      do n = 1,size(atom1)
         call rescale_displace_partition_(self,pt,wt,n, atom1)
         call V(mol,v_grid,nl_v_grid,pt)
         v_grid = wt*v_grid
         nl_v_grid(:,1) = wt*nl_v_grid(:,1)
         nl_v_grid(:,2) = wt*nl_v_grid(:,2)
         nl_v_grid(:,3) = wt*nl_v_grid(:,3)
         call make_grid_(shell%a,a_grid,pt)
         call make_grid_(shell%b,b_grid,pt)
         call make_nabla_grid_(shell%a,nabla_a_grid,pt)
         call make_nabla_grid_(shell%b,nabla_b_grid,pt)
         do a = 1,shell%a%n_comp
         do b = 1,shell%b%n_comp
            wt = a_grid(:,a)*b_grid(:,b)
            ans(a,b) = ans(a,b) + sum(wt*v_grid)
            wt = nabla_a_grid(:,a,1)*b_grid(:,b) + a_grid(:,a)*nabla_b_grid(:,b,1)
            ans(a,b) = ans(a,b) + sum(wt*nl_v_grid(:,1))
            wt = nabla_a_grid(:,a,2)*b_grid(:,b) + a_grid(:,a)*nabla_b_grid(:,b,2)
            ans(a,b) = ans(a,b) + sum(wt*nl_v_grid(:,2))
            wt = nabla_a_grid(:,a,3)*b_grid(:,b) + a_grid(:,a)*nabla_b_grid(:,b,3)
            ans(a,b) = ans(a,b) + sum(wt*nl_v_grid(:,3))
         end do
         end do
      end do
      call nullify_ptr_part_(atom1); call destroy_(atom1)
      call destroy_(b_grid); call destroy_(a_grid)
      call destroy_(nabla_b_grid); call destroy_(nabla_a_grid)
      call destroy_(v_grid); call destroy_(nl_v_grid)
      call destroy_(wt);  call destroy_(pt)

   end subroutine

   subroutine make_SO_matrix_elements_of(self,V,mol,shell,ans)
    type(dftgrid_type) :: self
    ! Integrate the spin orbit matrix elements of a multiplicative operator "V"
    ! for molecule "mol" between all gradient (nabla) components and shell
    ! components of two A.O. shells, given by the shell-pair "shell". The result
    ! of the integration is matrix "ans". Operator "V" is represented by a
    ! subroutine which returns "v_grid" on a set of "pts" for a given molecule
    ! "mol".
      interface
         subroutine V(mol,v_grid,pts)
   use TYPES_MODULE
            type(mol_type) :: mol
            real(kind=kind(1.0d0)), dimension(:) :: v_grid
            real(kind=kind(1.0d0)), dimension(:,:) :: pts
         end subroutine
      end interface
      type(mol_type) :: mol
      type(shell2_type) :: shell
      real(kind=kind(1.0d0)), dimension(:,:,:,:) :: ans
      type(atom_type), dimension(:), pointer :: atom1
      integer(kind=kind(1)) :: n_pt, n,k,l,a,b,atom_a,atom_b
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt
      real(kind=kind(1.0d0)), dimension(:), pointer :: wt, v_grid
      real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: a_grid,b_grid

      call ensure_(tonto,self%finalized,"DFTGRID:make_SO_matrix_elements_of ... call the set_grid_data routine")
      ans = 0.0d0
      n_pt = self%n_pts
      call create_(pt,n_pt,3);  call create_(wt,n_pt)
      call create_(v_grid,n_pt)
      call create_(a_grid,n_pt,shell%a%n_comp,3)
      call create_(b_grid,n_pt,shell%b%n_comp,3)
      call get_atom_grid_(self)
      atom_a = atom_index_from_pos_(mol%atom,shell%a%pos)
      atom_b = atom_index_from_pos_(mol%atom,shell%b%pos)
      if (atom_a==atom_b) then
         call create_(atom1,1)
         atom1(1) = mol%atom(atom_a)
      else
         call create_(atom1,2)
         atom1(1) = mol%atom(atom_a)
         atom1(2) = mol%atom(atom_b)
      end if
      do n = 1,size(atom1)
         call rescale_displace_partition_(self,pt,wt,n, atom1)
         call V(mol,v_grid,pt)
         v_grid = wt*v_grid
         call make_nabla_grid_(shell%a,a_grid,pt)
         call make_nabla_grid_(shell%b,b_grid,pt)
         do k = 1,3
         do l = 1,3
         do a = 1,shell%a%n_comp
         do b = 1,shell%b%n_comp
            wt = a_grid(:,a,k)*b_grid(:,b,l)
            ans(a,b,k,l) = ans(a,b,k,l) + sum(wt*v_grid)
         end do
         end do
         end do
         end do
      end do
      call nullify_ptr_part_(atom1); call destroy_(atom1)
      call destroy_(b_grid); call destroy_(a_grid)
      call destroy_(v_grid)
      call destroy_(wt);  call destroy_(pt)

   end subroutine

!  ************************
!  Grid generation routines
!  ************************

   subroutine get_grid(self,pt,wt,atom)
    type(dftgrid_type) :: self
    ! Get from an archive an entire dft integration grid, points and weights
    ! ("pt","wt") for a list of "atom" positions.
      real(kind=kind(1.0d0)), dimension(:,:) :: pt
      real(kind=kind(1.0d0)), dimension(:) :: wt
      type(atom_type), dimension(:) :: atom

      call set_name_(self%archive,"DFT_grid")
      if (.not. exists_(self%archive)) then
        call make_grid_(self,pt,wt,atom)
        call write_(self%archive,pt,wt)
      else
        call read_(self%archive,pt,wt)
      end if

   end subroutine

   subroutine make_grid(self,pt,wt,atom)
    type(dftgrid_type) :: self
    ! Make an entire dft integration grid, points and weights ("pt","wt")
    ! for a given a list of "atom" positions.
      real(kind=kind(1.0d0)), dimension(:,:) :: pt
      real(kind=kind(1.0d0)), dimension(:) :: wt
      type(atom_type), dimension(:) :: atom
      integer(kind=kind(1)) :: n_pt, a, n, n_atom

      call ensure_(tonto,self%finalized,"DFTGRID:make_grid ... call the set_grid_data routine")
      call ensure_(tonto,size(pt,1)==self%n_pts*size(atom),"DFTGRID:make_grid ... wrong dimension, pt")
      call ensure_(tonto,size(pt,2)==3,"DFTGRID:make_grid ... wrong size, pt")
      call ensure_(tonto,size(wt)==self%n_pts*size(atom),"DFTGRID:make_grid ... wrong dimension, wt")
      n_atom = size(atom)
      n_pt = self%n_pts
      call get_atom_grid_(self)
      n = 0
      do a = 1,n_atom
         call rescale_displace_partition_(self,pt(n+1:n+n_pt,:),wt(n+1:n+n_pt),a,atom)
         n = n + n_pt
      end do

   end subroutine

   subroutine rescale_displace_partition(self,pt,wt,a,atom)
    type(dftgrid_type) :: self
    ! Rescales the original single atom grid to adjust it to the atom size, then
    ! displaces the grid from the origin to the position of "atom" number "a",
    ! and partition it to work with all the other atoms, returning the grid
    ! ("pt","wt") = ("pt0" + disp, partition*"wt0") The weights are altered
    ! according to the Becke atomic partition function for the displaced grid
    ! centred on atom "a".
    ! NOTE : all the atoms in "atom" must be distinct!!!!!!
    ! References: A. D. Becke, J. Chem. Phys. 88 (1988) 2547.
    !             O. Treutler and R. Ahlrichs, J. Chem. Phys. 102 (1995) 346.
      real(kind=kind(1.0d0)), dimension(:,:) :: pt
      real(kind=kind(1.0d0)), dimension(:) :: wt
      integer(kind=kind(1)), intent(in) :: a
      type(atom_type), dimension(:), intent(in) :: atom
      integer(kind=kind(1)) :: n, m, n_atom, i, j, ii
      real(kind=kind(1.0d0)), dimension(3) :: disp,rij,rni,rnj
      real(kind=kind(1.0d0)) :: dij,dni,dnj,uij,vij, bsri, bsrj, chi, wij, aij, h, s, alpha
      real(kind=kind(1.0d0)), dimension(3) :: posi,posj
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: partition
      character(128) :: symbol

      call ensure_(tonto,self%finalized,"DFTGRID:rescale_displace_partition ... call the set_grid_data routine")
      call ensure_(tonto,size(pt,1)==self%n_pts,"DFTGRID:rescale_displace_partition ... pt incorrectly dimensioned")
      call ensure_(tonto,size(pt,2)==3,"DFTGRID:rescale_displace_partition ... pt incorrectly dimensioned")
      call ensure_(tonto,size(wt)==self%n_pts,"DFTGRID:rescale_displace_partition ... wt incorrectly dimensioned")
      call ensure_(tonto,a>=1 .and. a<= size(atom),"DFTGRID:rescale_displace_partition ... `a' argument out of range")
      call ensure_(tonto,associated(self%single_atom_points),"DFTGRID:rescale_displace_partition ... atom point grid not crea&
&ted")
      call ensure_(tonto,associated(self%single_atom_weights),"DFTGRID:rescale_displace_partition ... atom weight grid not cr&
&eated")
      call ensure_(tonto,size(self%single_atom_points,1)==self%n_pts,"DFTGRID:rescale_displace_partition ... points incorrect&
&ly dimensioned")
      call ensure_(tonto,size(self%single_atom_points,2)==3,"DFTGRID:rescale_displace_partition ... points incorrectly dimens&
&ioned")
      call ensure_(tonto,size(self%single_atom_weights)==self%n_pts,"DFTGRID:rescale_displace_partition ... weights incorrect&
&ly dimensioned")
      pt = self%single_atom_points
      wt = self%single_atom_weights
      symbol = chemical_symbol_(atom(a))      ! <-- Rescale the grid according to Becke's method
      if (symbol == "H") then
         alpha =      bragg_slater_radius_(atom(a))
      else
         alpha = 0.50d0*bragg_slater_radius_(atom(a))
      end if
      wt = alpha*alpha*alpha*wt
      pt = alpha*pt
      disp = atom(a)%pos                    ! <-- Displace the grid
      do n = 1,self%n_pts
          pt(n,:) = pt(n,:) + disp
      end do
      m = self%becke_m_partition_power          ! <-- Partition the grid, below
      n_atom = size(atom)                   ! Number of atoms
      call create_(partition,self%n_pts,n_atom)  ! Create partition array
      partition = 1.0d0
      do i = 1,n_atom
          bsri = bragg_slater_radius_(atom(i))
          posi = atom(i)%pos
          do j = 1,n_atom
              if (i==j) cycle
              bsrj = bragg_slater_radius_(atom(j))
              chi = bsri/bsrj
              wij = (chi - 1.0d0)/(chi + 1.0d0)
              aij = wij/(wij*wij - 1.0d0)
              posj = atom(j)%pos
              rij = posi-posj
              dij = sqrt(dot_product(rij,rij))
              do n = 1,self%n_pts
                  rni = pt(n,:) - posi
                  rnj = pt(n,:) - posj
                  dni = sqrt(dot_product(rni,rni))
                  dnj = sqrt(dot_product(rnj,rnj))
                  uij = (dni - dnj)/dij
                  vij = uij + aij*(1.0d0 - uij*uij)
                  h = vij
                  do ii = 1, m
                      h = 1.5d0*h - 0.50d0*h*h*h
                  end do
                   ! Optimise by doubling inside the do loop, and then halve it outside?
                  s = 0.50d0*(1.0d0 - h)
                   ! ===========================================
                  partition(n,i) = partition(n,i)*s
              end do
          end do
      end do
       ! Normalize the partitioning function and modify the grid weights
      do n = 1,self%n_pts
          wt(n) = wt(n)*partition(n,a)/sum(partition(n,:))
      end do
      call destroy_(partition)

   end subroutine

   subroutine rescale_displace_partition_1(self,pt,wt,a,pos,scale)
    type(dftgrid_type) :: self
    ! Rescales the original single atom grid according to the "scale" factors for
    ! that position "pos", then displaces the grid from the origin to the
    ! position "pos(a,:)", and partition it to work with all the other positions,
    ! returning the grid ("pt","wt") = ("pt0" + disp, partition*"wt0") The
    ! weights are altered according to a Becke-like atomic partition function
    ! where the scales are re-interpreted as mean extents.
    ! NOTE : all the positions in "atom" must be distinct!!!!!!
    ! References: A. D. Becke, J. Chem. Phys. 88 (1988) 2547.
    !             O. Treutler and R. Ahlrichs, J. Chem. Phys. 102 (1995) 346.
      real(kind=kind(1.0d0)), dimension(:,:) :: pt
      real(kind=kind(1.0d0)), dimension(:) :: wt
      integer(kind=kind(1)), intent(in) :: a
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pos
      real(kind=kind(1.0d0)), dimension(:) :: scale
      integer(kind=kind(1)) :: n, m, n_centers, i, j, ii
      real(kind=kind(1.0d0)), dimension(3) :: disp,rij,rni,rnj
      real(kind=kind(1.0d0)) :: dij,dni,dnj,uij,vij, bsri, bsrj, chi, wij, aij, h, s, alpha
      real(kind=kind(1.0d0)), dimension(3) :: posi,posj
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: partition

      call ensure_(tonto,self%finalized,"DFTGRID:rescale_displace_partition_1 ... call the set_grid_data routine")
      call ensure_(tonto,size(pt,1)==self%n_pts,"DFTGRID:rescale_displace_partition_1 ... pt incorrectly dimensioned")
      call ensure_(tonto,size(pt,2)==3,"DFTGRID:rescale_displace_partition_1 ... pt incorrectly dimensioned")
      call ensure_(tonto,size(wt)==self%n_pts,"DFTGRID:rescale_displace_partition_1 ... wt incorrectly dimensioned")
      call ensure_(tonto,a>=1 .and. a<= size(pos,1),"DFTGRID:rescale_displace_partition_1 ... `a' argument out of range")
      call ensure_(tonto,associated(self%single_atom_points),"DFTGRID:rescale_displace_partition_1 ... atom point grid not cr&
&eated")
      call ensure_(tonto,associated(self%single_atom_weights),"DFTGRID:rescale_displace_partition_1 ... atom weight grid not &
&created")
      call ensure_(tonto,size(self%single_atom_points,1)==self%n_pts,"DFTGRID:rescale_displace_partition_1 ... points incorre&
&ctly dimensioned")
      call ensure_(tonto,size(self%single_atom_points,2)==3,"DFTGRID:rescale_displace_partition_1 ... points incorrectly dime&
&nsioned")
      call ensure_(tonto,size(self%single_atom_weights)==self%n_pts,"DFTGRID:rescale_displace_partition_1 ... weights incorre&
&ctly dimensioned")
      pt = self%single_atom_points
      wt = self%single_atom_weights
       ! Rescale the grid according to Becke's method
      alpha = scale(a)
      wt = alpha*alpha*alpha*wt
      pt = alpha*pt
      disp = pos(a,:)                       ! <-- Displace the grid
      do n = 1,self%n_pts
          pt(n,:) = pt(n,:) + disp
      end do
      m = self%becke_m_partition_power          ! <-- Partition the grid, below
      n_centers = size(pos,1)                  ! Number of integration (atom) centres
      call create_(partition,self%n_pts,n_centers)  ! Create partition array
      partition = 1.0d0
      do i = 1,n_centers
          bsri = scale(i)
          posi = pos(i,:)
          do j = 1,n_centers
              if (i==j) cycle
              bsrj = scale(j)
              chi = bsri/bsrj
              wij = (chi - 1.0d0)/(chi + 1.0d0)
              aij = wij/(wij*wij - 1.0d0)
              posj = pos(j,:)
              rij = posi-posj
              dij = sqrt(dot_product(rij,rij))
              do n = 1,self%n_pts
                  rni = pt(n,:) - posi
                  rnj = pt(n,:) - posj
                  dni = sqrt(dot_product(rni,rni))
                  dnj = sqrt(dot_product(rnj,rnj))
                  uij = (dni - dnj)/dij
                  vij = uij + aij*(1.0d0 - uij*uij)
                  h = vij
                  do ii = 1, m
                      h = 1.5d0*h - 0.50d0*h*h*h
                  end do
                  s = 0.50d0*(1.0d0 - h)
                   ! ===========================================
                  partition(n,i) = partition(n,i)*s
              end do
          end do
      end do
       ! Normalize the partitioning function and modify the grid weights
      do n = 1,self%n_pts
          wt(n) = wt(n)*partition(n,a)/sum(partition(n,:))
      end do
      call destroy_(partition)

   end subroutine

   subroutine get_atom_grid(self)
    type(dftgrid_type) :: self
    ! Ensure the atom grid has been made.

      if (.not. associated(self%single_atom_points)) then
        call ensure_(tonto,.not. associated(self%single_atom_weights),"DFTGRID:get_atom_grid ... inconsistently allocated ato&
&m weights")
        call create_(self%single_atom_points,self%n_pts,3)
        call create_(self%single_atom_weights,self%n_pts)
       call make_atom_grid_(self,self%single_atom_points,self%single_atom_weights)
      end if

   end subroutine

   subroutine make_atom_grid(self,pt,wt)
    type(dftgrid_type) :: self
    ! Make the dft grid points "pt", and weights "wt" at the origin
    ! suitable for integrating a single atom. It is made as the
    ! direct product of a spherical grid and radial grid.
      real(kind=kind(1.0d0)), dimension(:,:) :: pt
      real(kind=kind(1.0d0)), dimension(:) :: wt
      integer(kind=kind(1)) :: i, j, k
      real(kind=kind(1.0d0)) :: pi4, pi4r2w
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: spherical_pt
      real(kind=kind(1.0d0)), dimension(:), pointer :: spherical_wt,radial_pt,radial_wt

      call ensure_(tonto,self%finalized,"DFTGRID:make_atom_grid ... call the set_grid_data routine")
      call ensure_(tonto,size(pt,1)==self%n_pts,"DFTGRID:make_atom_grid ... pt incorrectly dimensioned")
      call ensure_(tonto,size(pt,2)==3,"DFTGRID:make_atom_grid ... pt incorrectly dimensioned")
      call ensure_(tonto,size(wt)==self%n_pts,"DFTGRID:make_atom_grid ... wt incorrectly dimensioned")
       ! Allocate the spherical and radial grids
      call create_(spherical_pt,self%n_spherical_pts,3)
      call create_(spherical_wt,self%n_spherical_pts)
      call create_(radial_pt,self%n_radial_pts)
      call create_(radial_wt,self%n_radial_pts)
       ! Make the grids
      call make_spherical_grid_(self,spherical_pt,spherical_wt)
      call make_radial_grid_(self,radial_pt,radial_wt)
       ! Do the direct product
      pi4 = 4.0d0*3.141592653589793d0           ! Include the factor 4*pi*r^2 in the weights.
      k = 0
      do i = 1,self%n_radial_pts
         pi4r2w = pi4*radial_pt(i)*radial_pt(i)*radial_wt(i)
         do j = 1,self%n_spherical_pts
            k = k + 1
            wt(k)   = pi4r2w*spherical_wt(j)
            pt(k,:) = radial_pt(i)*spherical_pt(j,:)
         end do
      end do
      call destroy_(radial_wt)
      call destroy_(radial_pt)
      call destroy_(spherical_wt)
      call destroy_(spherical_pt)

   end subroutine

   subroutine make_radial_grid(self,pt,wt)
    type(dftgrid_type) :: self
    ! Make a one-dimensional radial array of coordinates "pt"
    ! and weights "wt"
     real(kind=kind(1.0d0)), dimension(:) :: pt,wt

     select case (self%radial_grid_kind)
       case ("gauss-chebyshev");   call make_gauss_chebyshev_grid_(self,pt,wt)
       case ("gauss_chebyshev");   call make_gauss_chebyshev_grid_(self,pt,wt)
       case ("euler-maclaurin");   call make_euler_maclaurin_grid_(self,pt,wt)
       case ("euler_maclaurin");   call make_euler_maclaurin_grid_(self,pt,wt)
       case default;               allocate(tonto%known_keywords(4))
       tonto%known_keywords(1) = "gauss-chebyshev"
       tonto%known_keywords(2) = "gauss_chebyshev"
       tonto%known_keywords(3) = "euler-maclaurin"
       tonto%known_keywords(4) = "euler_maclaurin"
       call unknown_(tonto,self%radial_grid_kind,"DFTGRID:make_radial_grid")
       deallocate(tonto%known_keywords)
     end select

   end subroutine

   subroutine make_gauss_chebyshev_grid(self,pt,wt)
    type(dftgrid_type) :: self
    ! Make a one-dimensional radial Gauss-Chebyshev array of
    ! coordinates "pt" and weights "wt".
      real(kind=kind(1.0d0)), dimension(:) :: pt,wt
      integer(kind=kind(1)) :: i, num
      real(kind=kind(1.0d0)) :: alpha, m, rm, pm, tm, pf, a, x

      alpha = self%gauss_chebyshev_alpha
      m     = self%gauss_chebyshev_m
      num   = self%radial_grid_order
      rm = 1.0d0/m
      pm = 1.0d0 + m
      tm = 1.0d0 - m
      pf = 3.141592653589793d0/num
      do i = 1, num
         a = cos(pf*(i-0.50d0))
          !a = cos(3.141592653589793d0*(2*i-1)/(2.0d0*num))
         x = (1.0d0 + a)/(1.0d0 - a)**rm
         pt(i) = alpha*x
         wt(i) = alpha*x*pf*(pm + a*tm)/(m*sqrt(1.0d0 - a*a))
      end do

   end subroutine

   subroutine make_euler_maclaurin_grid(self,pt,wt)
    type(dftgrid_type) :: self
    ! Make a one-dimensional radial Euler-Maclaurin array of
    ! coordinates "pt" and weights "wt"
      real(kind=kind(1.0d0)), dimension(:) :: pt,wt
      integer(kind=kind(1)) :: i, num
      real(kind=kind(1.0d0)) :: alpha, m, pm, tm, pf, n1, a, x

      alpha = self%euler_maclaurin_alpha
      m     = self%euler_maclaurin_m
      num   = self%radial_grid_order
      pm = 1.0d0 + m
      tm = 1.0d0 - m
      n1 = 1.0d0 + num
      pf = m/n1
      do i = 1, num
         a = i/n1
         x = (a**m)/(1.0d0 - a)**m
         pt(i) = alpha*x
         wt(i) = alpha*pf*(a**(-tm))/(1.0d0 - a)**pm
      end do

   end subroutine

   subroutine make_spherical_grid(self,pt,wt)
    type(dftgrid_type) :: self
    ! Make a three-dimensional spherical array of points "pt"
    ! and a one ! dimensional array of weights "wt".
    ! At the moment only spherical Lebedev grids are available
      real(kind=kind(1.0d0)), dimension(:,:) :: pt
      real(kind=kind(1.0d0)), dimension(:) :: wt

      select case (self%spherical_grid_kind)
         case ("lebedev");  call make_lebedev_grid_(self,pt,wt)
         case default;      allocate(tonto%known_keywords(1))
         tonto%known_keywords(1) = "lebedev"
         call unknown_(tonto,self%spherical_grid_kind,"DFTGRID:make_spherical_grid")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine make_lebedev_grid(self,pt,wt)
    type(dftgrid_type) :: self
    ! Make a three-dimensional spherical Lebedev array of points "pt"
    ! and a one dimensional array of weights "wt".
      real(kind=kind(1.0d0)), dimension(:,:) :: pt
      real(kind=kind(1.0d0)), dimension(:) :: wt
      integer(kind=kind(1)) :: m1, m2, m3, n1, n2, n3
      integer(kind=kind(1)) :: k, n
      real(kind=kind(1.0d0)) :: a1, a2, a3 ! coefficients of quadrature expansion
      real(kind=kind(1.0d0)), dimension(:), pointer :: b, c, d ! coefficients of quadrature expansion
      real(kind=kind(1.0d0)), dimension(:), pointer :: l, p, r, s ! used to generate coordinates of grid
      real(kind=kind(1.0d0)) :: m, q, w ! used to generate coordinates of grid
      real(kind=kind(1.0d0)) :: sr2, sr3

      pt = 0.0d0
      wt = 0.0d0
      call set_lebedev_data_(self,m1,m2,m3,n1,n2,n3)
      call create_(b,n1); call create_(c,n2); call create_(d,n3)
      call create_(l,n1)
      call create_(p,n2)
      call create_(r,n3); call create_(s,n3)
      select case (self%spherical_grid_order)
         case (9)
             !
             ! REF: V. I. Lebedev, Zh. Vyshisl. Mat. Mat. Fiz., 15 (1975) 48.
             !
            call ensure_(tonto,n1==0 .and. n2==1 .and. n3==0,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = 1.0d0/105.0d0
            a2   = 0.0d0
            a3   = 9.0d0/280.0d0
            c(1) = 1.0d0/35.0d0
            p(1) = 0.888073833977d0
         case (11)
             !
             ! REF: V. I. Lebedev, Zh. Vyshisl. Mat. Mat. Fiz., 15 (1975) 48.
             !
            call ensure_(tonto,n1==1 .and. n2==0 .and. n3==0,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = 4.0d0/315.0d0
            a2   = 64.0d0/2835.0d0
            a3   = 27.0d0/1280.0d0
            b(1) = 11.0d0*11.0d0*11.0d0*11.0d0/725760.0d0
            l(1) = 0.301511344578d0
             !
         case (13)
             !
             ! REF: V. I. Lebedev, Zh. Vyshisl. Mat. Mat. Fiz., 15 (1975) 48.
             !
            call ensure_(tonto,n1==2 .and. n2==1 .and. n3==0,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = 0.0138665921047d0
            a2   = 0.0d0
            a3   = 0.0d0
            b(1) = 0.0130509318626d0
            b(2) = 0.0132064232231d0
            c(1) = 0.0119426635549d0
            l(1) = 0.286640146767d0
            l(2) = 0.659905001656d0
            p(1) = 0.841991943785d0
         case (15)
             !
             ! REF: V. I. Lebedev, Zh. Vyshisl. Mat. Mat. Fiz., 15 (1975) 48.
             !
            call ensure_(tonto,n1==2 .and. n2==1 .and. n3==0,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = 0.0115440115441d0
            a2   = 0.0d0
            a3   = 0.0119439090859d0
            b(1) = 0.0111105557106d0
            b(2) = 0.0118765012945d0
            c(1) = 0.0118123037469d0
            l(1) = 0.369602846454d0
            l(2) = 0.694354006603d0
            p(1) = 0.927330657151d0
         case (17)
             !
             ! REF: V. I. Lebedev, Zh. Vyshisl. Mat. Mat. Fiz., 15 (1975) 48.
             !
            call ensure_(tonto,n1==3 .and. n2==1 .and. n3==0,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = 0.0d0
            a2   = 0.00200918797730d0
            a3   = 0.00988550016044d0
            b(1) = 0.00844068048232d0
            b(2) = 0.00987390742389d0
            b(3) = 0.00935732169000d0
            l(1) = 0.162263300152d0
            l(2) = 0.383386152638d0
            l(3) = 0.686647945709d0
            c(1) = 0.00969499636166d0
            p(1) = 0.878158910604d0
         case (19)
             !
             ! REF: V. I. Lebedev, Zh. Vyshisl. Mat. Mat. Fiz., 16 (1976) 293.
             !
            call ensure_(tonto,n1==3 .and. n2==0 .and. n3==1,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = 5.99631368862d-4
            a2   = 7.37299971862d-3
            a3   = 7.21051536014d-3
            b(1) = 7.57439415905d-3
            b(2) = 6.75382948631d-3
            b(3) = 7.11635549312d-3
            d(1) = 6.99108735330d-3
            l(1) = 0.157467667204d0
            l(2) = 0.417496122797d0
            l(3) = 0.676441040011d0
            r(1) = 0.882270011260d0
            s(1) = 0.140355381171d0
         case (23)
             !
             ! REF: V. I. Lebedev, Zh. Vyshisl. Mat. Mat. Fiz., 16 (1976) 293.
             !
            call ensure_(tonto,n1==4 .and. n2==1 .and. n3==1,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = 1.78234044724d-3
            a2   = 5.71690594998d-3
            a3   = 5.57338317884d-3
            b(1) = 5.51877146727d-3
            b(2) = 5.15823771181d-3
            b(3) = 5.60870408259d-3
            b(4) = 4.10677702817d-3
            c(1) = 5.05184606462d-3
            d(1) = 5.53024891623d-3
            l(1) = 0.444693317871d0
            l(2) = 0.289246562758d0
            l(3) = 0.671297344270d0
            l(4) = 0.129933544765d0
            p(1) = 0.938319218138d0
            r(1) = 0.836036015482d0
            s(1) = 0.159041710538d0
         case (25)
             !
             ! REF: V. I. Lebedev, Sibirsk. Mat. Zh. 18 (1977) 132.
             !
            call ensure_(tonto,n1==5 .and. n2==2 .and. n3==1,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = -5.52263991974d-2
            a2   = 0.0d0
            a3   = 4.45027460745d-3
            b(1) = 3.97640801805d-3
            b(2) = 4.40140065038d-3
            b(3) = 4.49684106792d-3
            b(4) = 5.04915345048d-3
            b(5) = 1.72454435055d-2
            c(1) = 5.19806986406d-3
            c(2) = 4.23108309536d-3
            d(1) = 4.69572097257d-3
            l(1) = 0.698190665845d0
            l(2) = 0.658740524346d0
            l(3) = 0.449204468740d0
            l(4) = 0.252041949021d0
            l(5) = 4.03854405009d-2
            p(1) = 0.935022745881d0
            p(2) = 0.812913653173d0
            r(1) = 0.486466535887d0
            s(1) = 0.843636521069d0
         case (27)
             !
             ! REF: V. I. Lebedev, Sibirsk. Mat. Zh. 18 (1977) 132.
             !
            call ensure_(tonto,n1==5 .and. n2==1 .and. n3==2,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = -1.31376912733d-3
            a2   = -2.52272870489d-3
            a3   = 4.18685388170d-3
            b(1) = 5.31516797782d-3
            b(2) = 4.25613135143d-3
            b(3) = 4.11248239441d-3
            b(4) = 3.59558489976d-3
            b(5) = 4.04714237709d-3
            c(1) = 4.22958270065d-3
            d(1) = 4.07146759383d-3
            d(2) = 4.08091422578d-3
            l(1) = 0.703937339159d0
            l(2) = 0.662033866370d0
            l(3) = 0.464744872642d0
            l(4) = 0.327742065497d0
            l(5) = 0.101252624857d0
            p(1) = 0.850650808352d0
            r(1) = 0.819343388819d0
            r(2) = 0.939227929750d0
            s(1) = 0.524493924092d0
            s(2) = 0.323348454269d0
         case (29)
             !
             ! REF: V. I. Lebedev, Sibirsk. Mat. Zh. 18 (1977) 132.
             !
            call ensure_(tonto,n1==6 .and. n2==2 .and. n3==2,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = 8.54591172878d-4
            a2   = 0.0d0
            a3   = 3.59911928502d-3
            b(1) = 3.65004580768d-3
            b(2) = 3.60482260142d-3
            b(3) = 3.57672966173d-3
            b(4) = 3.44978842429d-3
            b(5) = 3.10895312238d-3
            b(6) = 2.35210141366d-3
            c(1) = 3.60082093222d-3
            c(2) = 2.98234496317d-3
            d(1) = 3.57154055427d-3
            d(2) = 3.39231220501d-3
            l(1) = 0.701176641609d0
            l(2) = 0.656632941022d0
            l(3) = 0.472905413258d0
            l(4) = 0.351564034558d0
            l(5) = 0.221964523631d0
            l(6) = 0.0961830852303d0
            p(1) = 0.820326419828d0
            p(2) = 0.964408914879d0
            r(1) = 0.251003475177d0
            r(2) = 0.902442529533d0
            s(1) = 0.800072749407d0
            s(2) = 0.412772408317d0
         case (35)
             !
             ! REF: O. Treutler and R. Ahlrichs, J. Chem. Phys. 102 (1995) 346.
             !
            call ensure_(tonto,n1==7 .and. n2==2 .and. n3==4,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = 0.52659765761428065d-3
            a2   = 0.25482199909403521d-2
            a3   = 0.25123173709441058d-2
            b(1) = 0.25304038224001323d-2
            b(2) = 0.25132671684706878d-2
            b(3) = 0.25017251210647733d-2
            b(4) = 0.24453733047996786d-2
            b(5) = 0.23026944325620758d-2
            b(6) = 0.20142782609526094d-2
            b(7) = 0.14624950815475142d-2
            c(1) = 0.19109513147305082d-2
            c(2) = 0.24174423575419847d-2
            d(1) = 0.22366077071364263d-2
            d(2) = 0.24169300107381179d-2
            d(3) = 0.25122368647336706d-2
            d(4) = 0.24966440519292456d-2
            l(1) = 0.69093463105113458d0
            l(2) = 0.64566647095194987d0
            l(3) = 0.49143426555639500d0
            l(4) = 0.39272598223217649d0
            l(5) = 0.28612891787658218d0
            l(6) = 0.17748365242374568d0
            l(7) = 0.07568095866244468d0
            p(1) = 0.97764280892098723d0
            p(2) = 0.88181328936054412d0
            r(1) = 0.09921769971362576d0
            r(2) = 0.20548237125466495d0
            r(3) = 0.10680182513533723d0
            r(4) = 0.31042840327515130d0
            s(1) = 0.33443631695748371d0
            s(2) = 0.45023303874296735d0
            s(3) = 0.59051570309804130d0
            s(4) = 0.55501523681448068d0
         case (41)
             !
             ! REF: V. I. Lebedev, Russian Acad. Sci. Dokl. Math. 45 (1992) 587.
             !
            call ensure_(tonto,n1==9 .and. n2==3 .and. n3==6,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = 0.3095121295d-3
            a2   = 0.0d0
            a3   = 0.1852379698d-2
            b(1) = 0.9764331164d-3
            b(2) = 0.1384737234d-2
            b(3) = 0.1617210647d-2
            b(4) = 0.1749564657d-2
            b(5) = 0.1818471778d-2
            b(6) = 0.1846715956d-2
            b(7) = 0.1852028828d-2
            b(8) = 0.1858812585d-2
            b(9) = 0.1871790639d-2
            c(1) = 0.1300321685d-2
            c(2) = 0.1705153996d-2
            c(3) = 0.1857161196d-2
            d(1) = 0.1555213603d-2
            d(2) = 0.1802239128d-2
            d(3) = 0.1849830560d-2
            d(4) = 0.1713904507d-2
            d(5) = 0.1802658934d-2
            d(6) = 0.1842866472d-2
            l(1) = 0.6095034115d-1
            l(2) = 0.1459036449d0
            l(3) = 0.2384736701d0
            l(4) = 0.3317920736d0
            l(5) = 0.4215761784d0
            l(6) = 0.5044419707d0
            l(7) = 0.6372546939d0
            l(8) = 0.6807744066d0
            l(9) = 0.7040954938d0
            p(1) = 0.9850133350d0
            p(2) = 0.9180452877d0
            p(3) = 0.7911019296d0
            s(1) = 0.8213021581d-1
            s(2) = 0.8999205842d-1
            s(3) = 0.1816640840d0
            s(4) = 0.1720795225d0
            s(5) = 0.2634716655d0
            s(6) = 0.3518280927d0
            r(1) = 0.2778673190d0
            r(2) = 0.5033564271d0
            r(3) = 0.5984126497d0
            r(4) = 0.3791035407d0
            r(5) = 0.4742392842d0
            r(6) = 0.5610263808d0
         case (47)
             !
             ! REF: V. I. Lebedev, Russian Acad. Sci. Dokl. Math. 45 (1992) 587.
             !
            call ensure_(tonto,n1==10 .and. n2==3 .and. n3==9,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1    = 0.2192942090d-3
            a2    = 0.1436433617d-2
            a3    = 0.1421940344d-2
            b(1)  = 0.6798123510d-3
            b(2)  = 0.9913184235d-3
            b(3)  = 0.1180207833d-2
            b(4)  = 0.1296599602d-2
            b(5)  = 0.1365871427d-2
            b(6)  = 0.1402988604d-2
            b(7)  = 0.1418645563d-2
            b(8)  = 0.1421376741d-2
            b(9)  = 0.1423996475d-2
            b(10) = 0.1431554042d-2
            c(1)  = 0.9254401499d-3
            c(2)  = 0.1250239995d-2
            c(3)  = 0.1394365843d-2
            d(1)  = 0.1127089094d-2
            d(2)  = 0.1345753761d-2
            d(3)  = 0.1424957283d-2
            d(4)  = 0.1261523341d-2
            d(5)  = 0.1392547106d-2
            d(6)  = 0.1418761677d-2
            d(7)  = 0.1338366684d-2
            d(8)  = 0.1393700862d-2
            d(9)  = 0.1415914757d-2
            l(1)  = 0.5087204410d-1
            l(2)  = 0.1228198790d0
            l(3)  = 0.2026890814d0
            l(4)  = 0.2847745156d0
            l(5)  = 0.3656719078d0
            l(6)  = 0.4428264886d0
            l(7)  = 0.5140619627d0
            l(8)  = 0.6306401219d0
            l(9)  = 0.6716883332d0
            l(10) = 0.6979792685d0
            p(1)  = 0.9894775374d0
            p(2)  = 0.9407768787d0
            p(3)  = 0.8457493051d0
            s(1)  = 0.6944024393d-1
            s(2)  = 0.2269004109d0
            s(3)  = 0.8025574608d-1
            s(4)  = 0.1467999527d0
            s(5)  = 0.1571507769d0
            s(6)  = 0.2365702993d0
            s(7)  = 0.7714815844d-1
            s(8)  = 0.3062936666d0
            s(9)  = 0.3822477379d0
            r(1)  = 0.2355187894d0
            r(2)  = 0.4102182474d0
            r(3)  = 0.6214302417d0
            r(4)  = 0.3245284345d0
            r(5)  = 0.5224482189d0
            r(6)  = 0.6017546634d0
            r(7)  = 0.4346575516d0
            r(8)  = 0.4908826589d0
            r(9)  = 0.5648768149d0
         case (53)
             !
             ! REF: V. I. Lebedev, Russian Acad. Sci. Dokl. Math. 45 (1992) 587.
             !
            call ensure_(tonto,n1==12 .and. n2==4 .and. n3==12,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1    = 0.1438294190d-3
            a2    = 0.0d0
            a3    = 0.1125772288d-2
            b(1)  = 0.4948029342d-3
            b(2)  = 0.7357990108d-3
            b(3)  = 0.8889132771d-3
            b(4)  = 0.9888347838d-3
            b(5)  = 0.1053299681d-2
            b(6)  = 0.1092778807d-2
            b(7)  = 0.1114389394d-2
            b(8)  = 0.1123724788d-2
            b(9)  = 0.1125239325d-2
            b(10) = 0.1126153271d-2
            b(11) = 0.1130286931d-2
            b(12) = 0.1134986534d-2
            c(1)  = 0.6823367927d-3
            c(2)  = 0.9454158160d-3
            c(3)  = 0.1074429975d-2
            c(4)  = 0.1129300086d-2
            d(1)  = 0.8436884500d-3
            d(2)  = 0.1075255720d-2
            d(3)  = 0.1108577236d-2
            d(4)  = 0.9566475323d-3
            d(5)  = 0.1080663250d-2
            d(6)  = 0.1126797131d-2
            d(7)  = 0.1022568715d-2
            d(8)  = 0.1108960267d-2
            d(9)  = 0.1122790653d-2
            d(10) = 0.1032401847d-2
            d(11) = 0.1107249382d-2
            d(12) = 0.1121780048d-2
            l(1)  = 0.4292963545d-1
            l(2)  = 0.1051426854d0
            l(3)  = 0.1750024867d0
            l(4)  = 0.2477653379d0
            l(5)  = 0.3206567123d0
            l(6)  = 0.3916520749d0
            l(7)  = 0.4590825874d0
            l(8)  = 0.5214563888d0
            l(9)  = 0.6253170244d0
            l(10) = 0.6637926744d0
            l(11) = 0.6910410398d0
            l(12) = 0.7052907007d0
            p(1)  = 0.9923235654d0
            p(2)  = 0.9557815124d0
            p(3)  = 0.8827859807d0
            p(4)  = 0.7737784472d0
            s(1)  = 0.5974048614d-1
            s(2)  = 0.1375760408d0
            s(3)  = 0.3391016526d0
            s(4)  = 0.1271675191d0
            s(5)  = 0.2693120740d0
            s(6)  = 0.1419786452d0
            s(7)  = 0.6709284600d-1
            s(8)  = 0.7057738183d-1
            s(9)  = 0.2783888477d0
            s(10) = 0.1979578938d0
            s(11) = 0.2087307061d0
            s(12) = 0.4055122137d0
            r(1)  = 0.2029128752d0
            r(2)  = 0.4602621942d0
            r(3)  = 0.5030673999d0
            r(4)  = 0.2817606422d0
            r(5)  = 0.4331561291d0
            r(6)  = 0.6256167328d0
            r(7)  = 0.3798395291d0
            r(8)  = 0.5517505421d0
            r(9)  = 0.6029619156d0
            r(10) = 0.3589606329d0
            r(11) = 0.5348666438d0
            r(12) = 0.5674997546d0
         case (59)
             !
             ! REF: V. I. Lebedev, Russian Acad. Sci. Dokl. Math. 50 (1995) 283.
             !
            call ensure_(tonto,n1==13 .and. n2==4 .and. n3==16,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
             !
             ! **** NOTE THIS TABLE IS .not. YET COMPLETE! ****
             !
            a1    = 0.110518923327d-3
            a2    = 0.920523273809d-3
            a3    = 0.913315978645d-3
            b(1)  = 0.369042189802d-3
            b(2)  = 0.560399092868d-3
            b(3)  = 0.686529762928d-3
            b(4)  = 0.772033855115d-3
            b(5)  = 0.830154595889d-3
            b(6)  = 0.868669255018d-3
            b(7)  = 0.892707628585d-3
            b(8)  = 0.906082023857d-3
            b(9)  = 0.911977725494d-3
            b(10) = 0.912872013860d-3
            b(11) = 0.913071493569d-3
            b(12) = 0.915287378455d-3
            b(13) = 0.918743627432d-3
            c(1)  = 0.517697731297d-3
            c(2)  = 0.733114368210d-3
            c(3)  = 0.846323283638d-3
            c(4)  = 0.903112269425d-3
            d(1)  = 0.648577845316d-3
            d(2)  = 0.743503091098d-3
            d(3)  = 0.799852789184d-3
            d(4)  = 0.810173149747d-3
            d(5)  = 0.848338957459d-3
            d(6)  = 0.855629925731d-3
            d(7)  = 0.880320867974d-3
            d(8)  = 0.881104818243d-3
            d(9)  = 0.885028234127d-3
            d(10) = 0.902134229904d-3
            d(11) = 0.901009167711d-3
            d(12) = 0.902269293843d-3
            d(13) = 0.915801617469d-3
            d(14) = 0.913157800319d-3
            d(15) = 0.910781357948d-3
            d(16) = 0.910576025897d-3
            l(1)  = 0.371263644966d-1
            l(2)  = 0.914006041226d-1
            l(3)  = 0.153107785247d0
            l(4)  = 0.218092889166d0
            l(5)  = 0.283987453220d0
            l(6)  = 0.349117600096d0
            l(7)  = 0.412143146144d0
            l(8)  = 0.471899362715d0
            l(9)  = 0.527314545284d0
            l(10) = 0.620947533244d0
            l(11) = 0.656972271186d0
            l(12) = 0.684178830907d0
            l(13) = 0.701260433012d0
            p(1)  = 0.107238221548d0
            p(2)  = 0.258206895950d0
            p(3)  = 0.417275295531d0
            p(4)  = 0.570036691179d0
            s(1)  = 0.982798601826d0
            s(2)  = 0.962424923033d0
            s(3)  = 0.940200799413d0
            s(4)  = 0.932082204014d0
            s(5)  = 0.904367419939d0
            s(6)  = 0.891240756007d0
            s(7)  = 0.867643562846d0
            s(8)  = 0.858197998604d0
            s(9)  = 0.839675362405d0
            s(10) = 0.816528856402d0
            s(11) = 0.801546937078d0
            s(12) = 0.777356306907d0
            s(13) = 0.766162121390d0
            s(14) = 0.755358414353d0
            s(15) = 0.734430575756d0
            s(16) = 0.704383718402d0
            r(1)  = 0.177177402262d0
            r(2)  = 0.247571646343d0
            r(3)  = 0.335461628907d0
            r(4)  = 0.317361524661d0
            r(5)  = 0.409026842709d0
            r(6)  = 0.385429115067d0
            r(7)  = 0.493222118485d0
            r(8)  = 0.478532067592d0
            r(9)  = 0.450742259316d0
            r(10) = 0.563212302076d0
            r(11) = 0.543430356969d0
            r(12) = 0.512351848642d0
            r(13) = 0.639427963475d0
            r(14) = 0.626980550902d0
            r(15) = 0.603116169310d0
            r(16) = 0.569370249847d0
         case default
            call die_(tonto,"DFTGRID:make_lebedev_grid ... No grid with order "// trim(to_str_(self%spherical_grid_order)))
      end select
      n = 0  ! should be the total number of spherical points.  Use this for cross-checking.
      if (m1 /= 0) then                                     ! Points for a1, number = 6
         ! point(:) = (/ 1.0d0, 0.0d0, 0.0d0 /)
         ! .pm_perm_point(point, set, n)
        call pm_perm_point_11_(self,1.0d0, pt(n+1:n+6,:))
        wt(n+1:n+6) = a1
        n = n + 6
      end if
      if (m2 /= 0) then                                     ! Points for a2, number = 12
         ! point(:) = (/ sr2, sr2, 0.0d0 /)
         ! .pm_perm_point(point, set, n)
        sr2 = 1.0d0/sqrt(2.0d0)
        call pm_perm_point_12_(self,sr2, pt(n+1:n+12,:))
        wt(n+1:n+12) = a2
        n = n + 12
      end if
      if (m3 /= 0) then                                     ! Points for a3, number = 8
         ! point(:) = (/ sr3, sr3, sr3 /)
         ! .pm_perm_point(point, set, n)
        sr3 = 1.0d0/sqrt(3.0d0)
        call pm_perm_point_13_(self,sr3, pt(n+1:n+8,:))
        wt(n+1:n+8) = a3
        n = n + 8
      end if
      do k = 1, n1                                          ! Points for b(k), number = 24
        m = sqrt(1.0d0 - 2.0d0*l(k)*l(k))
         ! point(:) = (/ l(k), l(k), m /)
         ! .pm_perm_point(point, set, n)
        call pm_perm_point_22_(self,m, l(k), pt(n+1:n+24,:))
        wt(n+1:n+24) = b(k)
        n = n + 24
      end do
      do k = 1, n2                                          ! Points for c(k), number = 24
        q = sqrt(1.0d0 - p(k)*p(k))
         ! point(:) = (/ p(k), q, 0.0d0 /)
         ! .pm_perm_point(point, set, n)
        call pm_perm_point_21_(self,p(k), q, pt(n+1:n+24,:))
        wt(n+1:n+24) = c(k)
        n = n + 24
      end do
      do k = 1, n3                                          ! Points for d, number = 48
        w = sqrt(1.0d0 - r(k)*r(k) - s(k)*s(k))
         ! point(:) = (/ r(k), s(k), w /)
         ! .pm_perm_point(point, set, n)
        call pm_perm_point_31_(self,r(k), s(k), w, pt(n+1:n+48,:))
        wt(n+1:n+48) = d(k)
        n = n + 48
      end do
      call destroy_(b); call destroy_(c); call destroy_(d)
      call destroy_(l)
      call destroy_(p)
      call destroy_(r); call destroy_(s)
      call ensure_(tonto,n==self%n_spherical_pts,"DFTGRID:make_lebedev_grid ... Incorrect number of Lebedev grid points")

   end subroutine

   pure subroutine pm_perm_point_11(self,a, set)
    type(dftgrid_type) :: self
    ! Permutates (a, 0, 0) according to the octahedral group
    ! "set" must be at least: dimension (1:3, 1:6)
      intent(in) :: self
      real(kind=kind(1.0d0)), intent(in) :: a
      real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: set
      set(1,:) = (/    a, 0.0d0, 0.0d0 /)
      set(2,:) = (/   -a, 0.0d0, 0.0d0 /)
      set(3,:) = (/ 0.0d0,    a, 0.0d0 /)
      set(4,:) = (/ 0.0d0,   -a, 0.0d0 /)
      set(5,:) = (/ 0.0d0, 0.0d0,    a /)
      set(6,:) = (/ 0.0d0, 0.0d0,   -a /)

   end subroutine

   pure subroutine pm_perm_point_12(self,a, set)
    type(dftgrid_type) :: self
    ! Permutates (a, a, 0) according to the octahedral group
    ! "set" must be at least: dimension (1:3, 1:12)
      intent(in) :: self
      real(kind=kind(1.0d0)), intent(in) :: a
      real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: set
      set(1,:)  =  (/    a,    a, 0.0d0 /)
      set(2,:)  =  (/   -a,    a, 0.0d0 /)
      set(3,:)  =  (/    a,   -a, 0.0d0 /)
      set(4,:)  =  (/   -a,   -a, 0.0d0 /)
      set(5,:)  =  (/    a, 0.0d0,    a /)
      set(6,:)  =  (/   -a, 0.0d0,    a /)
      set(7,:)  =  (/    a, 0.0d0,   -a /)
      set(8,:)  =  (/   -a, 0.0d0,   -a /)
      set(9,:)  =  (/ 0.0d0,    a,    a /)
      set(10,:) =  (/ 0.0d0,   -a,    a /)
      set(11,:) =  (/ 0.0d0,    a,   -a /)
      set(12,:) =  (/ 0.0d0,   -a,   -a /)

   end subroutine

   pure subroutine pm_perm_point_13(self,a, set)
    type(dftgrid_type) :: self
    ! Permutates (a, a, a) according to the octahedral group
    ! "set" must be at least: dimension (1:3, 1:8)
      intent(in) :: self
      real(kind=kind(1.0d0)), intent(in) :: a
      real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: set
      set(1,:)  =  (/  a,  a,  a /)
      set(2,:)  =  (/  a, -a,  a /)
      set(3,:)  =  (/  a,  a, -a /)
      set(4,:)  =  (/  a, -a, -a /)
      set(5,:)  =  (/ -a,  a,  a /)
      set(6,:)  =  (/ -a, -a,  a /)
      set(7,:)  =  (/ -a,  a, -a /)
      set(8,:)  =  (/ -a, -a, -a /)

   end subroutine

   pure subroutine pm_perm_point_21(self,a, b, set)
    type(dftgrid_type) :: self
    ! Permutates (a, b, 0), a.ne.b, according to the octahedral group
    ! "set" must be at least: dimension (1:3, 1:24)
      intent(in) :: self
      real(kind=kind(1.0d0)), intent(in) :: a, b
      real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: set
      set(1,:)  =  (/    a,    b, 0.0d0 /)
      set(2,:)  =  (/   -a,    b, 0.0d0 /)
      set(3,:)  =  (/    a,   -b, 0.0d0 /)
      set(4,:)  =  (/   -a,   -b, 0.0d0 /)
      set(5,:)  =  (/    b,    a, 0.0d0 /)
      set(6,:)  =  (/   -b,    a, 0.0d0 /)
      set(7,:)  =  (/    b,   -a, 0.0d0 /)
      set(8,:)  =  (/   -b,   -a, 0.0d0 /)
      set(9,:)  =  (/    a, 0.0d0,    b /)
      set(10,:) =  (/   -a, 0.0d0,    b /)
      set(11,:) =  (/    a, 0.0d0,   -b /)
      set(12,:) =  (/   -a, 0.0d0,   -b /)
      set(13,:) =  (/    b, 0.0d0,    a /)
      set(14,:) =  (/   -b, 0.0d0,    a /)
      set(15,:) =  (/    b, 0.0d0,   -a /)
      set(16,:) =  (/   -b, 0.0d0,   -a /)
      set(17,:) =  (/ 0.0d0,    a,    b /)
      set(18,:) =  (/ 0.0d0,   -a,    b /)
      set(19,:) =  (/ 0.0d0,    a,   -b /)
      set(20,:) =  (/ 0.0d0,   -a,   -b /)
      set(21,:) =  (/ 0.0d0,    b,    a /)
      set(22,:) =  (/ 0.0d0,   -b,    a /)
      set(23,:) =  (/ 0.0d0,    b,   -a /)
      set(24,:) =  (/ 0.0d0,   -b,   -a /)

   end subroutine

   pure subroutine pm_perm_point_22(self,a, b, set)
    type(dftgrid_type) :: self
    ! Permutates (a, b, b),  a.ne.b, according to the octahedral group
    ! "set" must be at least: dimension (1:3, 1:24)
      intent(in) :: self
      real(kind=kind(1.0d0)), intent(in) :: a, b
      real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: set
      set(1,:)  =  (/  a, b, b /)
      set(2,:)  =  (/  a,-b, b /)
      set(3,:)  =  (/  a, b,-b /)
      set(4,:)  =  (/  a,-b,-b /)
      set(5,:)  =  (/ -a, b, b /)
      set(6,:)  =  (/ -a,-b, b /)
      set(7,:)  =  (/ -a, b,-b /)
      set(8,:)  =  (/ -a,-b,-b /)
      set(9,:)  =  (/  b, a, b /)
      set(10,:) =  (/ -b, a, b /)
      set(11,:) =  (/  b, a,-b /)
      set(12,:) =  (/ -b, a,-b /)
      set(13,:) =  (/  b,-a, b /)
      set(14,:) =  (/ -b,-a, b /)
      set(15,:) =  (/  b,-a,-b /)
      set(16,:) =  (/ -b,-a,-b /)
      set(17,:) =  (/  b, b, a /)
      set(18,:) =  (/ -b, b, a /)
      set(19,:) =  (/  b,-b, a /)
      set(20,:) =  (/ -b,-b, a /)
      set(21,:) =  (/  b, b,-a /)
      set(22,:) =  (/ -b, b,-a /)
      set(23,:) =  (/  b,-b,-a /)
      set(24,:) =  (/ -b,-b,-a /)

   end subroutine

   pure subroutine pm_perm_point_31(self,a, b, c, set)
    type(dftgrid_type) :: self
    ! Permutates (a, b, c), a.ne.b & a.ne.c & b.ne.c, according to the octahedral
    ! group "set" must be at least: dimension (1:3, 1:48)
      intent(in) :: self
      real(kind=kind(1.0d0)), intent(in) :: a, b, c
      real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: set
      set(1,:)  =  (/  a, b, c /)
      set(2,:)  =  (/ -a, b, c /)
      set(3,:)  =  (/  a,-b, c /)
      set(4,:)  =  (/ -a,-b, c /)
      set(5,:)  =  (/  b, a, c /)
      set(6,:)  =  (/ -b, a, c /)
      set(7,:)  =  (/  b,-a, c /)
      set(8,:)  =  (/ -b,-a, c /)
      set(9,:)  =  (/  a, c, b /)
      set(10,:) =  (/ -a, c, b /)
      set(11,:) =  (/  a, c,-b /)
      set(12,:) =  (/ -a, c,-b /)
      set(13,:) =  (/  b, c, a /)
      set(14,:) =  (/ -b, c, a /)
      set(15,:) =  (/  b, c,-a /)
      set(16,:) =  (/ -b, c,-a /)
      set(17,:) =  (/  c, a, b /)
      set(18,:) =  (/  c,-a, b /)
      set(19,:) =  (/  c, a,-b /)
      set(20,:) =  (/  c,-a,-b /)
      set(21,:) =  (/  c, b, a /)
      set(22,:) =  (/  c,-b, a /)
      set(23,:) =  (/  c, b,-a /)
      set(24,:) =  (/  c,-b,-a /)
      set(25,:) =  (/  a, b,-c /)
      set(26,:) =  (/ -a, b,-c /)
      set(27,:) =  (/  a,-b,-c /)
      set(28,:) =  (/ -a,-b,-c /)
      set(29,:) =  (/  b, a,-c /)
      set(30,:) =  (/ -b, a,-c /)
      set(31,:) =  (/  b,-a,-c /)
      set(32,:) =  (/ -b,-a,-c /)
      set(33,:) =  (/  a,-c, b /)
      set(34,:) =  (/ -a,-c, b /)
      set(35,:) =  (/  a,-c,-b /)
      set(36,:) =  (/ -a,-c,-b /)
      set(37,:) =  (/  b,-c, a /)
      set(38,:) =  (/ -b,-c, a /)
      set(39,:) =  (/  b,-c,-a /)
      set(40,:) =  (/ -b,-c,-a /)
      set(41,:) =  (/ -c, a, b /)
      set(42,:) =  (/ -c,-a, b /)
      set(43,:) =  (/ -c, a,-b /)
      set(44,:) =  (/ -c,-a,-b /)
      set(45,:) =  (/ -c, b, a /)
      set(46,:) =  (/ -c,-b, a /)
      set(47,:) =  (/ -c, b,-a /)
      set(48,:) =  (/ -c,-b,-a /)

   end subroutine

!  **************
!  Output methods
!  **************

   subroutine put(self,output)
    type(dftgrid_type) :: self
    ! Put out the type(dftgrid_type) to file "output"
      type(textfile_type), target, optional :: output
      type(textfile_type), pointer :: out

      if (present(output)) then
        out => output
      else
        out => stdout
      end if
      call flush_(out)
      call text_(out,"DFTGRID output:")
      call text_(out," ")
      call show_(out,"Spherical grid kind       =", self%spherical_grid_kind)
      call show_(out,"Spherical grid order      =", self%spherical_grid_order)
      call show_(out,"No. of spherical points   =", self%n_spherical_pts)
      call text_(out," ")
      call show_(out,"Radial grid kind          =", self%radial_grid_kind)
      call show_(out,"Radial grid order         =", self%radial_grid_order)
      call show_(out,"No. of radial points      =", self%n_radial_pts)
      call text_(out," ")
      call show_(out,"No. of grid points        =", self%n_pts)
      call text_(out," ")
      call show_(out,"Becke m partition power     =", self%becke_m_partition_power)
      call show_(out,"Gauss-Chebyshev alpha value =", self%gauss_chebyshev_alpha)
      call show_(out,"Gauss-Chebyshev m     value =", self%gauss_chebyshev_m)
      call show_(out,"Euler-Maclaurin alpha value =", self%euler_maclaurin_alpha)
      call show_(out,"Euler-Maclaurin m     value =", self%euler_maclaurin_m)

   end subroutine

!*******************************************************************************
!                  DFT functionals and derivatives
!*******************************************************************************

   subroutine d_u_lda_exchange_functional(self,p_a,p_b,dfdp_a,dfdp_b,alpha)
    type(dftgrid_type) :: self
    ! Return the derivatives of the local density exchange functional.
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: p_a,p_b
     real(kind=kind(1.0d0)), dimension(:) :: dfdp_a,dfdp_b
     real(kind=kind(1.0d0)), intent(in) :: alpha
     real(kind=kind(1.0d0)) :: const

     const = - 3.0d0 * alpha * (3.0d0/(4.0d0*3.141592653589793d0))**0.33333333333333333333333d0
     dfdp_a = const* p_a**0.33333333333333333333333d0
     dfdp_b = const* p_b**0.33333333333333333333333d0

   end subroutine

   subroutine u_lda_exchange_functional(self,p_a,p_b,f,alpha)
    type(dftgrid_type) :: self
    ! Return the values of the local density exchange functional.
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: p_a,p_b
     real(kind=kind(1.0d0)), dimension(:) :: f
     real(kind=kind(1.0d0)), intent(in) :: alpha
     real(kind=kind(1.0d0)) :: const

     const = - 9.0d0/4.0d0 * alpha * (3.0d0/(4.0d0*3.141592653589793d0))**0.33333333333333333333333d0
     f = const * (p_a**(4.0d0/3.0d0) + p_b**(4.0d0/3.0d0))

   end subroutine

   subroutine u_becke88_exchange_functional(self,p_a,p_b,np_a,np_b,f)
    type(dftgrid_type) :: self
    ! Return the values of the Becke 88 exchange functional.
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: p_a,p_b
     real(kind=kind(1.0d0)), dimension(:) :: f
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: np_a,np_b
     real(kind=kind(1.0d0)) :: beta,fac,xa,xb,paft,pbft
     integer(kind=kind(1)) :: i

     beta = 0.0042   ! beta parameter
     fac = -3.0d0/2.0d0 * (3.0d0/(4.0d0*3.141592653589793d0))**0.33333333333333333333333d0
     do i=1,size(p_a)
       paft = (p_a(i)) ** (4.0d0/3.0d0)
       pbft = (p_b(i)) ** (4.0d0/3.0d0)
       xa = max(sqrt(dot_product(np_a(i,:),np_a(i,:))) / paft,10.0d0**(-20))
       xb = max(sqrt(dot_product(np_b(i,:),np_b(i,:))) / pbft,10.0d0**(-20))
       f(i) = paft * (fac - beta*xa*xa/(1.0d0+6.0d0*beta*xa*arcsinh_(xa))) &
            + pbft * (fac - beta*xb*xb/(1.0d0+6.0d0*beta*xb*arcsinh_(xb)))
     end do

   end subroutine

  subroutine d_u_b88_exchange_functional(self,p_a,p_b,np_a,np_b,local_a,local_b,non_local_a,non_local_b)
    type(dftgrid_type) :: self
    ! Return the derivatives of the Becke 88 exchange functional.
    ! These equations are essentially the same as in the appendix of JCP 98(7)
    ! 5612-5626.
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: p_a,p_b
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: np_a,np_b
     real(kind=kind(1.0d0)), dimension(:) :: local_a,local_b
     real(kind=kind(1.0d0)), dimension(:,:) :: non_local_a,non_local_b
     real(kind=kind(1.0d0)), dimension(3) :: npa,npb
     real(kind=kind(1.0d0)) :: c,d,e,pa,pb,pa_third,pb_third,xa,xb,ka,kb,xa2,xb2
     real(kind=kind(1.0d0)) :: ka2,kb2,za,zb,beta,nla,nlb,paft,pbft
     integer(kind=kind(1)) :: i,n_pt

     beta = 0.0042   ! beta parameter
     n_pt = size(p_a)
     c = beta*0.50d0
     d = - (6.0d0/3.141592653589793d0)**0.33333333333333333333333d0
     e = 4.0d0*0.33333333333333333333333d0*beta
     do i=1,n_pt
       pa = p_a(i)
       pb = p_b(i)
       npa = np_a(i,:)
       npb = np_b(i,:)
       pa_third = pa**(0.33333333333333333333333d0)
       pb_third = pb**(0.33333333333333333333333d0)
       paft = pa*pa_third
       pbft = pb*pb_third
       xa = max(sqrt(dot_product(npa,npa)) / paft,10.0d0**(-20))
       xb = max(sqrt(dot_product(npb,npb)) / pbft,10.0d0**(-20))
       xa2 = xa*xa
       xb2 = xb*xb
       ka = 1.0d0 + 6.0d0 * beta * xa * arcsinh_(xa)
       kb = 1.0d0 + 6.0d0 * beta * xb * arcsinh_(xb)
       ka2 = ka*ka
       kb2 = kb*kb
       za = 6.0d0*beta*xa2/sqrt(1.0d0+xa2)
       zb = 6.0d0*beta*xb2/sqrt(1.0d0+xb2)
       local_a(i) = d * pa_third + e * pa_third * xa2 * (1.0d0-za) / ka2
       local_b(i) = d * pb_third + e * pb_third * xb2 * (1.0d0-zb) / kb2
       if (xa < 10.0d0**(-19)) then
         non_local_a(i,:) = 0.0d0
       else
         nla = beta * (za-1.0d0-ka)/(ka2*paft)
         non_local_a(i,:) = nla*npa(:)
       end if
       if (xb < 10.0d0**(-19)) then
         non_local_b(i,:) = 0.0d0
       else
         nlb = beta * (zb-1.0d0-kb)/(kb2*pbft)
         non_local_b(i,:) = nlb*npb(:)
       end if
     end do

   end subroutine

   subroutine u_gill96_exchange_functional(self,p_a,p_b,np_a,np_b,f)
    type(dftgrid_type) :: self
    ! Return the values of the Gill 96 exchange functional.
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: p_a,p_b
     real(kind=kind(1.0d0)), dimension(:) :: f
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: np_a,np_b
     real(kind=kind(1.0d0)) :: alpha,fac,xa,xb,paft,pbft
     integer(kind=kind(1)) :: i

     alpha = -3.0d0/2.0d0 * (3.0d0/(4.0d0*3.141592653589793d0))**0.33333333333333333333333d0
     fac = 1.0d0/137
     do i=1,size(p_a)
       paft = (p_a(i)) ** (4.0d0/3.0d0)
       pbft = (p_b(i)) ** (4.0d0/3.0d0)
       xa = max(sqrt(dot_product(np_a(i,:),np_a(i,:))) / paft,10.0d0**(-20))
       xb = max(sqrt(dot_product(np_b(i,:),np_b(i,:))) / pbft,10.0d0**(-20))
       f(i) = paft * (alpha - fac*xa*sqrt(xa)) + pbft * (alpha - fac*xb*sqrt(xb))
     end do

   end subroutine

  subroutine d_u_gill96_exchange_functional(self,p_a,p_b,np_a,np_b,local_a,local_b,non_local_a,non_local_b)
    type(dftgrid_type) :: self
   ! Return the derivatives of the Gill 96 exchange functional.
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: p_a,p_b
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: np_a,np_b
     real(kind=kind(1.0d0)), dimension(:) :: local_a,local_b
     real(kind=kind(1.0d0)), dimension(:,:) :: non_local_a,non_local_b
     real(kind=kind(1.0d0)), dimension(3) :: npa,npb
     real(kind=kind(1.0d0)) :: alpha,ft,fac,fac1,pa,pb,pa_third,pb_third,xa,xb,xa2,xb2,nla,nlb
     integer(kind=kind(1)) :: i,n_pt

     n_pt = size(p_a)
     alpha = -3.0d0/2.0d0 * (3.0d0/(4.0d0*3.141592653589793d0))**0.33333333333333333333333d0
     ft = 4.0d0/3.0d0
     fac = 1.0d0/274
     fac1 = -3.0d0/548
     do i=1,n_pt
       pa = p_a(i)
       pb = p_b(i)
       npa = np_a(i,:)
       npb = np_b(i,:)
       pa_third = pa**(0.33333333333333333333333d0)
       pb_third = pb**(0.33333333333333333333333d0)
       xa = max(sqrt(dot_product(npa,npa))/(pa*pa_third),10.0d0**(-20))
       xb = max(sqrt(dot_product(npb,npb))/(pb*pb_third),10.0d0**(-20))
       xa2 = sqrt(xa)
       xb2 = sqrt(xb)
       local_a(i) = ft*pa_third*(alpha+fac*xa*xa2)
       local_b(i) = ft*pb_third*(alpha+fac*xb*xb2)
       if (xa < 10.0d0**(-19)) then
         non_local_a(i,:) = 0.0d0
       else
         nla = 2.0d0*fac1*xa2/(xa*pa*pa_third)
         non_local_a(i,:) = nla*npa(:)
       end if
       if (xb < 10.0d0**(-19)) then
         non_local_b(i,:) = 0.0d0
       else
         nlb = 2.0d0*fac1*xb2/(xb*pb*pb_third)
         non_local_b(i,:) = nlb*npb(:)
       end if
     end do

  end subroutine

   subroutine u_lyp_correlation_functional(self,p_a,p_b,np_a,np_b,f)
    type(dftgrid_type) :: self
    ! Return the values of the Lee-Yang-Parr functional.
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: p_a,p_b
     real(kind=kind(1.0d0)), dimension(:) :: f
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: np_a,np_b
     real(kind=kind(1.0d0)) :: c_f,a,b,c,d,fac,pa,pb,p,npanpa,npbnpb,npanpb,p_third
     real(kind=kind(1.0d0)) :: gamma,a_b_omega,delta,pa_pb_n,fi
     integer(kind=kind(1)) :: i,start,step

     c_f = (3.0d0/10.0d0)*(3*3.141592653589793d0*3.141592653589793d0)**(0.33333333333333333333333d0+0.33333333333333333333333&
&d0)
     a = 0.04918
     b = 0.132
     c = 0.2533
     d = 0.349
     fac = 2.0d0**(11*0.33333333333333333333333d0)*c_f
     start = 1 + this_proc_(tonto_parallel)
     step = n_proc_(tonto_parallel)
     if (tonto_parallel%do_parallel) then
       f = 0.0d0
     end if
     do i=start,size(p_a),step
       pa = p_a(i)
       pb = p_b(i)
       p = pa + pb
       npanpa = dot_product(np_a(i,:),np_a(i,:))
       npbnpb = dot_product(np_b(i,:),np_b(i,:))
       npanpb = dot_product(np_a(i,:),np_b(i,:))
       p_third = p**0.33333333333333333333333d0
       gamma = 1.0d0 + d/p_third
       a_b_omega = a*b*exp(-c/p_third)/(gamma*p_third**11)
       delta = (c+d/gamma)/p_third
       pa_pb_n=pa*pb/9.0d0
       fi = -a * 4.0d0*pa*pb/(p*gamma)
       fi = fi - a_b_omega*fac*pa*pb*(pa**(8.0d0*0.33333333333333333333333d0)+pb**(8.0d0*0.33333333333333333333333d0))
       fi = fi + a_b_omega*(pa_pb_n*(4.0d0*delta-1.0d0-(11*pa+pb*delta)/p) + pb*pb)*npanpa
       fi = fi + a_b_omega*(pa_pb_n*(4.0d0*delta-1.0d0-(11*pb+pa*delta)/p) + pa*pa)*npbnpb
       fi = fi + a_b_omega*(12/9.0d0*p*p - pa_pb_n*(47-7.0d0*delta))*npanpb
       f(i) = fi
     end do
     call sum_vectors_(tonto_parallel,f)

   end subroutine

   subroutine d_u_lyp_correlation_functional(self,p_a,p_b,np_a,np_b,local_a,local_b,non_local_a,non_local_b)
    type(dftgrid_type) :: self
    ! Return the derivatives of the LYP correlation functional.
    ! These equations are essentially the same as in the appendix of JCP 98(7)
    ! 5612-5626.
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: p_a,p_b
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: np_a,np_b
     real(kind=kind(1.0d0)), dimension(:) :: local_a,local_b
     real(kind=kind(1.0d0)), dimension(:,:) :: non_local_a,non_local_b
     real(kind=kind(1.0d0)), dimension(3) :: npa,npb
     real(kind=kind(1.0d0)) :: a,b,c,d,e,c_f,ab9
     real(kind=kind(1.0d0)) :: pa,pb,p,p2,pa2,pb2,papb,p_third,npanpa,npbnpb,npanpb
     real(kind=kind(1.0d0)) :: gamma_inv,mu,abw9,abw27,delta,pa83,pb83,tmp1,tmp2
     real(kind=kind(1.0d0)) :: dfdnpanpb,dfdnpanpa,dfdnpbnpb
     integer(kind=kind(1)) :: i

     c_f = (3.0d0/10.0d0)*(3*3.141592653589793d0*3.141592653589793d0)**(0.33333333333333333333333d0+0.33333333333333333333333&
&d0)
     a = 0.04918
     b = 0.132
     c = 0.2533
     d = 0.349
     e = 2.0d0**(11*0.33333333333333333333333d0) * 9.0d0 * c_f
     ab9 = a*b/9.0d0

     do i=1,size(p_a)
       pa = p_a(i)
       pb = p_b(i)
       npa = np_a(i,:)
       npb = np_b(i,:)
       npanpa = dot_product(npa,npa)
       npbnpb = dot_product(npb,npb)
       npanpb = dot_product(npa,npb)
       p = pa + pb
       p_third = p**(0.33333333333333333333333d0)
       gamma_inv = 1.0d0/(1.0d0 + d / p_third)
       mu=d*gamma_inv/p_third
       p2=p*p
       abw9 = ab9*exp(-c/p_third) * p_third/(p2*p2) * gamma_inv
       abw27 = abw9*0.33333333333333333333333d0
       delta = c/p_third + mu
       pa2=pa*pa
       pb2=pb*pb
       pa83=pa2*pa/pa**0.33333333333333333333333d0
       pb83=pb2*pb/pb**0.33333333333333333333333d0
       tmp1 = pa*pb/p*(7*mu*mu+delta*(7*delta-131)+517)
       tmp2 = (delta-11)/p*(pb83+pa83)
       papb=pa*pb

       local_a(i) = -4*a*pb*gamma_inv/p2*(pb+0.33333333333333333333333d0*mu*pa) &
         +abw27*npanpb*(pa*(12*delta-60)+pb*(33*delta-201) + tmp1) &
         +abw27*npanpa*pb/p2*(96*pa2-160*papb-102*pb2 &
          +(-48*pa2-4*papb+18*pb2)*delta+pa*(4*p-pb)*(delta*delta+mu*mu)) &
         +abw27*npbnpb/p2*(-45*pa2*pa+17*pa2*pb+180*pa*pb2-36*pb2*pb &
          +(9*pa2*pa-19*pa2*pb-42*pa*pb2+12*pb2*pb)*delta &
          +papb*(4*p-pa)*(delta*delta+mu*mu)) &
         -abw27*pb*e*(pa*tmp2+11*pa83+3*pb83)
       local_b(i) = -4*a*pa*gamma_inv/p2*(pa+0.33333333333333333333333d0*mu*pb) &
         +abw27*npanpb*(pb*(12*delta-60)+pa*(33*delta-201) + tmp1) &
         +abw27*npbnpb*pa/p2*(96*pb2-160*papb-102*pa2 &
          +(-48*pb2-4*papb+18*pa2)*delta+pb*(4*p-pa)*(delta*delta+mu*mu)) &
         +abw27*npanpa/p2*(-45*pb2*pb+17*pb2*pa+180*pb*pa2-36*pa2*pa &
          +(9*pb2*pb-19*pb2*pa-42*pb*pa2+12*pa2*pa)*delta &
          +papb*(4*p-pb)*(delta*delta+mu*mu)) &
         -abw27*pa*e*(pb*tmp2+11*pb83+3*pa83)

       dfdnpanpb = abw9*(12*p2-papb*(47-7*delta))
       dfdnpanpa = abw9*(9.0d0*pb2+papb*(4.0d0*delta-1.0d0-(11*pa+pb*delta)/p))
       dfdnpbnpb = abw9*(9.0d0*pa2+papb*(4.0d0*delta-1.0d0-(11*pb+pa*delta)/p))
       non_local_a(i,:) = 2.0d0*dfdnpanpa*npa+dfdnpanpb*npb
       non_local_b(i,:) = 2.0d0*dfdnpbnpb*npb+dfdnpanpb*npa
     end do

   end subroutine

!*******************************************************************************
!                  DFT functionals and derivatives
!*******************************************************************************

   subroutine d_r_lda_exchange_functional(self,p,local,alpha)
    type(dftgrid_type) :: self
    ! Return the derivatives of the local density exchange functional.
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: p
     real(kind=kind(1.0d0)), dimension(:), intent(out) :: local
     real(kind=kind(1.0d0)), intent(in) :: alpha
     real(kind=kind(1.0d0)) :: const

     const = - 6.0d0 * alpha * (3.0d0/(8.0d0*3.141592653589793d0))**0.33333333333333333333333d0
     local = const * p**0.33333333333333333333333d0

   end subroutine

   subroutine r_lda_exchange_functional(self,p,f,alpha)
    type(dftgrid_type) :: self
    ! Return the values of the local density exchange functional.
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: p
     real(kind=kind(1.0d0)), dimension(:), intent(out) :: f
     real(kind=kind(1.0d0)), intent(in) :: alpha
     real(kind=kind(1.0d0)) :: const

     const = - 9.0d0/16 * alpha * (3.0d0/3.141592653589793d0)**0.33333333333333333333333d0
     f = const * p**(4.0d0/3.0d0)

   end subroutine

   subroutine r_becke88_exchange_functional(self,p,np,f)
    type(dftgrid_type) :: self
    ! Return the values of the Becke 88 exchange functional.
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: p
     real(kind=kind(1.0d0)), dimension(:), intent(out) :: f
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: np
     real(kind=kind(1.0d0)) :: beta,fac,x,pft
     integer(kind=kind(1)) :: i

     beta = 0.0042   ! beta parameter
     fac = -3.0d0/2.0d0 * (3.0d0/(4.0d0*3.141592653589793d0))**0.33333333333333333333333d0
     do i=1,size(p)
       pft = (0.50d0*p(i)) ** (4.0d0/3.0d0)
       x = 0.50d0 * max(sqrt(dot_product(np(i,:),np(i,:))) / pft,10.0d0**(-20))
       f(i) = 2.0d0 * pft * (fac - beta*x*x/(1.0d0+6.0d0*beta*x*arcsinh_(x)))
     end do

   end subroutine

  subroutine d_r_b88_exchange_functional(self,p,np,local,non_local)
    type(dftgrid_type) :: self
    ! Return the derivatives of the Becke 88 exchange functional.
    ! These equations are essentially the same as in the appendix of JCP 98(7)
    ! 5612-5626.
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: p
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: np
     real(kind=kind(1.0d0)), dimension(:) :: local
     real(kind=kind(1.0d0)), dimension(:,:) :: non_local
     real(kind=kind(1.0d0)), dimension(3) :: np_i
     real(kind=kind(1.0d0)) :: c,d,e,p_i,p_i_third,x,k,x2,k2,z,beta,nl,p_i_ft
     integer(kind=kind(1)) :: i

     beta = 0.0042   ! beta parameter
     c = beta*0.50d0
     d = - (6.0d0/3.141592653589793d0)**0.33333333333333333333333d0
     e = 4.0d0*0.33333333333333333333333d0*beta
     do i=1,size(p)
       p_i = 0.50d0*p(i)
       np_i = 0.50d0*np(i,:)
       p_i_third = p_i**(0.33333333333333333333333d0)
       p_i_ft = p_i*p_i_third
       x = max(sqrt(dot_product(np_i,np_i)) / p_i_ft,10.0d0**(-20))
       x2 = x*x
       k = 1.0d0 + 6.0d0 * beta * x * arcsinh_(x)
       k2 = k*k
       z = 6.0d0*beta*x2/sqrt(1.0d0+x2)
       local(i) = d * p_i_third + e * p_i_third * x2 * (1.0d0-z) / k2
       if (x < 10.0d0**(-19)) then
         non_local(i,:) = 0.0d0
       else
         nl = beta * (z-1.0d0-k)/(k2*p_i_ft)
         non_local(i,:) = nl*np_i(:)
       end if
     end do

   end subroutine

  subroutine d_r_gill96_exchange_functional(self,p,np,local,non_local)
    type(dftgrid_type) :: self
   ! Return the derivatives of the Gill 96 exchange functional.
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: p
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: np
     real(kind=kind(1.0d0)), dimension(:), intent(out) :: local
     real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: non_local
     real(kind=kind(1.0d0)), dimension(3) :: np_i
     real(kind=kind(1.0d0)) :: alpha,ft,fac,fac1,p_i,p_third,x,x2,nl
     integer(kind=kind(1)) :: i

     alpha = -3.0d0/2.0d0 * (3.0d0/(4.0d0*3.141592653589793d0))**0.33333333333333333333333d0
     ft = 4.0d0/3.0d0
     fac = 1.0d0/274
     fac1 = -3.0d0/548
     do i=1,size(p)
       p_i = 0.50d0*p(i)
       np_i = 0.50d0*np(i,:)
       p_third = p_i**(0.33333333333333333333333d0)
       x = max(sqrt(dot_product(np_i,np_i))/(p_i*p_third),10.0d0**(-20))
       x2 = sqrt(x)
       local(i) = ft*p_third*(alpha+fac*x*x2)
       if (x < 10.0d0**(-19)) then
         non_local(i,:) = 0.0d0
       else
         nl = 2.0d0*fac1*x2/(x*p_i*p_third)
         non_local(i,:) = nl*np_i(:)
       end if
     end do

  end subroutine

   subroutine r_gill96_exchange_functional(self,p,np,f)
    type(dftgrid_type) :: self
    ! Return the values of the Gill 96 exchange functional.
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: p
     real(kind=kind(1.0d0)), dimension(:), intent(out) :: f
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: np
     real(kind=kind(1.0d0)), dimension(3) :: np_i
     real(kind=kind(1.0d0)) :: alpha,fac,x,pft
     integer(kind=kind(1)) :: i

     alpha = -3.0d0/2.0d0 * (3.0d0/(4.0d0*3.141592653589793d0))**0.33333333333333333333333d0
     fac = 1.0d0/137
     do i=1,size(p)
       pft = (0.50d0*p(i)) ** (4.0d0/3.0d0)
       np_i = 0.50d0*np(i,:)
       x = max(sqrt(dot_product(np_i,np_i)) / pft,10.0d0**(-20))
       f(i) = 2.0d0*pft * (alpha - fac*x*sqrt(x))
     end do

   end subroutine

   subroutine r_lyp_correlation_functional(self,p,np,f)
    type(dftgrid_type) :: self
    ! Return the values of the Lee-Yang-Parr functional.
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: p
     real(kind=kind(1.0d0)), dimension(:), intent(out) :: f
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: np
     real(kind=kind(1.0d0)) :: c_f,a,b,c,d,fac,p_i,npnp,p_third
     real(kind=kind(1.0d0)) :: gamma,a_b_omega,delta,p2,fi
     integer(kind=kind(1)) :: i,start,step

     c_f = (3.0d0/10.0d0)*(3*3.141592653589793d0*3.141592653589793d0)**(0.33333333333333333333333d0+0.33333333333333333333333&
&d0)
     a = 0.04918
     b = 0.132
     c = 0.2533
     d = 0.349
     fac = 2.0d0**(11*0.33333333333333333333333d0)*c_f
     start = 1 + this_proc_(tonto_parallel)
     step = n_proc_(tonto_parallel)
     if (tonto_parallel%do_parallel) then
       f = 0.0d0
     end if
     do i=start,size(p),step
       p_i = 0.50d0*p(i)
       p2 = p_i*p_i
       npnp = 0.25d0*dot_product(np(i,:),np(i,:))
       p_third = (2.0d0*p_i)**0.33333333333333333333333d0
       gamma = 1.0d0 + d/p_third
       a_b_omega = a*b*exp(-c/p_third)/(gamma*p_third**11)
       delta = (c+d/gamma)/p_third
       fi = -a * 2.0d0*p_i/gamma &
            +a_b_omega*p2*npnp*(6+14*delta)/9.0d0 &
            -a_b_omega*fac*2.0d0*p_i**(14*0.33333333333333333333333d0)
       f(i) = fi
     end do
     call sum_vectors_(tonto_parallel,f)

   end subroutine

   subroutine d_r_lyp_correlation_functional(self,p,np,local,non_local)
    type(dftgrid_type) :: self
    ! Return the derivatives of the LYP correlation functional.
    ! These equations are essentially the same as in the appendix of JCP 98(7)
    ! 5612-5626.
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: p
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: np
     real(kind=kind(1.0d0)), dimension(:) :: local
     real(kind=kind(1.0d0)), dimension(:,:) :: non_local
     real(kind=kind(1.0d0)), dimension(3) :: npa
     real(kind=kind(1.0d0)) :: a,b,c,d,e,c_f,ab9
     real(kind=kind(1.0d0)) :: pa,pa_third,p_third,npanpa
     real(kind=kind(1.0d0)) :: gamma_inv,mu,abw9_pa,delta
     integer(kind=kind(1)) :: i

     c_f = (3.0d0/10.0d0)*(3*3.141592653589793d0*3.141592653589793d0)**(0.33333333333333333333333d0+0.33333333333333333333333&
&d0)
     a = 0.04918
     b = 0.132
     c = 0.2533
     d = 0.349
     e = (2.0d0)**(11*0.33333333333333333333333d0) * c_f
     ab9 = a*b/9.0d0

     do i=1,size(p)
       pa = 0.50d0*p(i)
       npa = 0.50d0*np(i,:)
       npanpa = dot_product(npa,npa)
       pa_third = pa**(0.33333333333333333333333d0)
       p_third = 2.0d0**(0.33333333333333333333333d0)*pa_third
       gamma_inv = 1.0d0/(1.0d0 + d / p_third)
       mu=d*gamma_inv/p_third
       abw9_pa = (0.50d0)**4 * ab9*exp(-c/p_third) * p_third/(pa*pa*pa) * gamma_inv
       delta = c/p_third + mu

       local(i) = -a*gamma_inv*(1.0d0+0.33333333333333333333333d0*mu) &
         +abw9_pa*npanpa*(7.0d0/3*(mu*mu+delta*delta)-13*delta-5) &
         -abw9_pa*e*(3*delta+9)*pa*pa*pa/pa_third
       non_local(i,:) = abw9_pa*pa*npa*(6+14*delta)
     end do

   end subroutine

end
