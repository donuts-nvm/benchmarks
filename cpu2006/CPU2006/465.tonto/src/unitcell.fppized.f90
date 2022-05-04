!-------------------------------------------------------------------------------
!
! UNITCELL: Data structure for a crystal unit cell
!
! $Id: unitcell.foo,v 1.6.2.6 2003/11/13 05:33:02 reaper Exp $
!
! Copyright (C) Dylan Jayatilaka, Daniel Grimwood, 1999
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
!-------------------------------------------------------------------------------

module UNITCELL_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: norm_
   use REALVEC_MODULE, only: rotate_by_
   use REALVEC_MODULE, only: convert_from_

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

   use REAL_MODULE, only: arccos_
   use REAL_MODULE, only: to_units_

   use CIF_MODULE, only: read_item_
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

   public    read_CIF_
   interface read_CIF_
      module procedure read_CIF
   end interface

   private    make_reciprocal_matrix_
   interface make_reciprocal_matrix_
      module procedure make_reciprocal_matrix
   end interface

   public    update_
   interface update_
      module procedure update
   end interface

   public    put_CX_
   interface put_CX_
      module procedure put_CX
   end interface

   private    read_gamma_
   interface read_gamma_
      module procedure read_gamma
   end interface

   public    make_info_
   interface make_info_
      module procedure make_info
   end interface

   public    read_keywords_
   interface read_keywords_
      module procedure read_keywords
   end interface

   public    change_from_fractional_
   interface change_from_fractional_
      module procedure change_from_fractional
      module procedure change_from_fractional_1
   end interface

   public    a_star_
   interface a_star_
      module procedure a_star
   end interface

   private    read_alpha_
   interface read_alpha_
      module procedure read_alpha
   end interface

   public    process_keyword_
   interface process_keyword_
      module procedure process_keyword
   end interface

   public    put_
   interface put_
      module procedure put
   end interface

   public    gamma_
   interface gamma_
      module procedure gamma
   end interface

   public    c_star_
   interface c_star_
      module procedure c_star
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   private    read_beta_
   interface read_beta_
      module procedure read_beta
   end interface

   public    beta_star_
   interface beta_star_
      module procedure beta_star
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    a_
   interface a_
      module procedure a
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    b_
   interface b_
      module procedure b
   end interface

   public    alpha_
   interface alpha_
      module procedure alpha
   end interface

   public    c_
   interface c_
      module procedure c
   end interface

   private    read_a_
   interface read_a_
      module procedure read_a
   end interface

   private    read_angles_
   interface read_angles_
      module procedure read_angles
   end interface

   private    make_direct_U_matrix_
   interface make_direct_U_matrix_
      module procedure make_direct_U_matrix
   end interface

   private    read_b_
   interface read_b_
      module procedure read_b
   end interface

   private    make_reciprocal_U_matrix_
   interface make_reciprocal_U_matrix_
      module procedure make_reciprocal_U_matrix
   end interface

   private    read_c_
   interface read_c_
      module procedure read_c
   end interface

   public    alpha_star_
   interface alpha_star_
      module procedure alpha_star
   end interface

   public    beta_
   interface beta_
      module procedure beta
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    gamma_star_
   interface gamma_star_
      module procedure gamma_star
   end interface

   public    read_junk_
   interface read_junk_
      module procedure read_junk
   end interface

   private    read_lengths_
   interface read_lengths_
      module procedure read_lengths
   end interface

   private    make_direct_matrix_
   interface make_direct_matrix_
      module procedure make_direct_matrix
   end interface

   public    b_star_
   interface b_star_
      module procedure b_star
   end interface

   public    read_units_
   interface read_units_
      module procedure read_units
   end interface

   private    make_volume_
   interface make_volume_
      module procedure make_volume
   end interface

   public    change_into_fractional_
   interface change_into_fractional_
      module procedure change_into_fractional
      module procedure change_into_fractional_1
   end interface

contains

!  **************************
!  Create and destroy methods
!  **************************

   subroutine create(self)
    type(unitcell_type) :: self
    ! Create the object
      pointer :: self

      nullify(self)
      allocate(self)

      call set_defaults_(self)

   end subroutine

   subroutine destroy(self)
    type(unitcell_type) :: self
    ! Destroy the object
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

   subroutine set_defaults(self)
    type(unitcell_type) :: self
    ! Set up a default crystal object

      self%angle        = (/90.0d0,90.0d0,90.0d0/)
      call convert_from_(self%angle,"degree")
      self%length       = (/10.0d0,10.0d0,10.0d0/)
      call make_info_(self)

   end subroutine

   subroutine create_copy(self,unitcell)
    type(unitcell_type) :: self
    ! Create a copy of "unitcell"
     pointer :: self
     type(unitcell_type), intent(in) :: unitcell

     call create_(self)
     call copy_(self,unitcell)

   end subroutine

   subroutine copy(self,unitcell)
    type(unitcell_type) :: self
    ! Set self to be "unitcell"
     type(unitcell_type), intent(in) :: unitcell

     self = unitcell

   end subroutine

!  ***************
!  Basic cell info
!  ***************

   function alpha(self) result(res)
    type(unitcell_type) :: self
    ! Return the alpha angle, in radians.
     real(kind=kind(1.0d0)) :: res

     call ensure_(tonto,self%info_made,"UNITCELL:alpha ... cell info not made")
     res = self%angle(1)

   end function

   function beta(self) result(res)
    type(unitcell_type) :: self
    ! Return the beta angle, in radians.
     real(kind=kind(1.0d0)) :: res

     call ensure_(tonto,self%info_made,"UNITCELL:beta ... cell info not made")
     res = self%angle(2)

   end function

   function gamma(self) result(res)
    type(unitcell_type) :: self
    ! Return the gamma angle, in radians.
     real(kind=kind(1.0d0)) :: res

     call ensure_(tonto,self%info_made,"UNITCELL:gamma ... cell info not made")
     res = self%angle(3)

   end function

   function alpha_star(self) result(res)
    type(unitcell_type) :: self
    ! Return the alpha reciprocal lattice angle, in radians.
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)) :: tmp,alpha,beta,gamma

     call ensure_(tonto,self%info_made,"UNITCELL:alpha_star ... cell info not made")
     alpha = self%angle(1); beta  = self%angle(2); gamma = self%angle(3)
     tmp = (cos(beta)*cos(gamma)-cos(alpha))/(sin(beta)*sin(gamma))
     res = arccos_(tmp)

   end function

   function beta_star(self) result(res)
    type(unitcell_type) :: self
    ! Return the beta reciprocal lattice angle, in radians.
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)) :: tmp,alpha,beta,gamma

     call ensure_(tonto,self%info_made,"UNITCELL:beta_star ... cell info not made")
     alpha = self%angle(1); beta  = self%angle(2); gamma = self%angle(3)
     tmp = (cos(gamma)*cos(alpha)-cos(beta))/(sin(gamma)*sin(alpha))
     res = arccos_(tmp)

   end function

   function gamma_star(self) result(res)
    type(unitcell_type) :: self
    ! Return the gamma reciprocal lattice angle, in radians.
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)) :: tmp,alpha,beta,gamma

     call ensure_(tonto,self%info_made,"UNITCELL:gamma_star ... cell info not made")
     alpha = self%angle(1); beta  = self%angle(2); gamma = self%angle(3)
     tmp = (cos(alpha)*cos(beta)-cos(gamma))/(sin(alpha)*sin(beta))
     res = arccos_(tmp)

   end function

   function a(self) result(res)
    type(unitcell_type) :: self
    ! Return the a cell length, in bohr.
     real(kind=kind(1.0d0)) :: res

     call ensure_(tonto,self%info_made,"UNITCELL:a ... cell info not made")
     res = self%length(1)

   end function

   function b(self) result(res)
    type(unitcell_type) :: self
    ! Return the b cell length, in bohr.
     real(kind=kind(1.0d0)) :: res

     call ensure_(tonto,self%info_made,"UNITCELL:b ... cell info not made")
     res = self%length(2)

   end function

   function c(self) result(res)
    type(unitcell_type) :: self
    ! Return the c cell length, in bohr.
     real(kind=kind(1.0d0)) :: res

     call ensure_(tonto,self%info_made,"UNITCELL:c ... cell info not made")
     res = self%length(3)

   end function

   function a_star(self) result(res)
    type(unitcell_type) :: self
    ! Return the a reciprocal lattice length, in bohr.
     real(kind=kind(1.0d0)) :: res

     call ensure_(tonto,self%info_made,"UNITCELL:a_star ... cell info not made")
     res = self%length(2) * self%length(3) * sin(self%angle(1)) / self%volume

   end function

   function b_star(self) result(res)
    type(unitcell_type) :: self
    ! Return the b reciprocal lattice length, in bohr.
     real(kind=kind(1.0d0)) :: res

     call ensure_(tonto,self%info_made,"UNITCELL:b_star ... cell info not made")
     res = self%length(3) * self%length(1) * sin(self%angle(2)) / self%volume

   end function

   function c_star(self) result(res)
    type(unitcell_type) :: self
    ! Return the c reciprocal lattice length, in bohr.
     real(kind=kind(1.0d0)) :: res

     call ensure_(tonto,self%info_made,"UNITCELL:c_star ... cell info not made")
     res = self%length(1) * self%length(2) * sin(self%angle(3)) / self%volume

   end function

!  **************************
!  Make cell axis/volume info
!  **************************

   subroutine make_info(self)
    type(unitcell_type) :: self
    ! Calculate the various unit cell axis matrices.

      self%info_made = .false.
      call make_volume_(self)
      call make_direct_matrix_(self)
      call make_reciprocal_matrix_(self)
      call make_direct_U_matrix_(self)
      call make_reciprocal_U_matrix_(self)
      self%info_made = .true.

   end subroutine

   subroutine make_volume(self)
    type(unitcell_type) :: self
    ! Calculate the cell volume
      real(kind=kind(1.0d0)) :: a,b,c,ca,cb,cg,sb

      a = self%length(1)
      b = self%length(2)
      c = self%length(3)
      ca = cos(self%angle(1))
      cb = cos(self%angle(2))
      cg = cos(self%angle(3))
      sb = sin(self%angle(2))
      self%volume = a*b*c*sqrt(1.0d0-ca**2-cb**2-cg**2+2.0d0*ca*cb*cg)

   end subroutine

   subroutine make_direct_matrix(self)
    type(unitcell_type) :: self
    ! Calculate the direct cell matrices (i.e. cell axes) in units of BOHRS.
      real(kind=kind(1.0d0)) :: v,a,b,c,ca,cb,cg,sb

      a = self%length(1)
      b = self%length(2)
      c = self%length(3)
      ca = cos(self%angle(1))
      cb = cos(self%angle(2))
      cg = cos(self%angle(3))
      sb = sin(self%angle(2))
      v = self%volume
       ! Direct cell matrix
      self%direct_matrix(1,1) = a
      self%direct_matrix(1,2) = b*cg
      self%direct_matrix(1,3) = c*cb
      self%direct_matrix(2,1) = 0.0d0
      self%direct_matrix(2,2) = v/(a*c*sb)
      self%direct_matrix(2,3) = 0.0d0
      self%direct_matrix(3,1) = 0.0d0
      self%direct_matrix(3,2) = b*(ca-cg*cb)/sb
      self%direct_matrix(3,3) = c*sb

   end subroutine

   subroutine make_reciprocal_matrix(self)
    type(unitcell_type) :: self
    ! Calculate the reciprocal cell matrices (i.e. reciprocal cell axes) in units
    ! of 1/BOHRS. Also calculate the inverse direct cell matrix.
      real(kind=kind(1.0d0)) :: v,a,b,c,ca,cb,cg,sb

      a = self%length(1)
      b = self%length(2)
      c = self%length(3)
      ca = cos(self%angle(1))
      cb = cos(self%angle(2))
      cg = cos(self%angle(3))
      sb = sin(self%angle(2))
      v = self%volume
       ! Reciprocal cell matrix
      self%reciprocal_matrix(1,1) = 1.0d0/a
      self%reciprocal_matrix(1,2) = 0.0d0
      self%reciprocal_matrix(1,3) = 0.0d0
      self%reciprocal_matrix(2,1) = b*c*(ca*cb-cg)/sb/v
      self%reciprocal_matrix(2,2) = a*c*sb/v
      self%reciprocal_matrix(2,3) = a*b*(cb*cg-ca)/sb/v
      self%reciprocal_matrix(3,1) = -cb/a/sb
      self%reciprocal_matrix(3,2) = 0.0d0
      self%reciprocal_matrix(3,3) = 1.0d0/c/sb
      self%inverse_matrix = transpose(self%reciprocal_matrix)

   end subroutine

   subroutine make_direct_U_matrix(self)
    type(unitcell_type) :: self
    ! Return the transformation matrix which changes the thermal tensor
    ! from the crystal axis system into the cartesian axis system.
    ! See comments for reciprocal_U_tensor_matrix below.
      real(kind=kind(1.0d0)) :: len
      integer(kind=kind(1)) :: i

      do i = 1,3
         len = norm_(self%reciprocal_matrix(:,i))
         self%direct_U_matrix(i,:) = len*self%direct_matrix(:,i)
      end do

   end subroutine

   subroutine make_reciprocal_U_matrix(self)
    type(unitcell_type) :: self
    ! Return the transformation matrix which changes the thermal tensor
    ! from the cartesian axis system into the crystal axis system.
    ! The thermal tensor in the crystal axis system U_{ij} is defined
    ! by the temperature factor expansion:
    !         TF = exp ( -2\pi^2 U_{ij} h_i h_j a^*_i a^*_j )
    ! where h are the Miller indices and a^* are the reciprocal lattice
    ! constants (in bohr^{-2}). This is as used by systems like Xtal.
    ! The thermal tensor in the cartesian axis system U_{ij} is defined
    ! by the temperature factor expansion:
    !         TF = exp ( -0.5 U_{ij} k_i k_j )
    ! where k = 2\pi B h, and B is the reciprocal cell matrix.
      real(kind=kind(1.0d0)) :: len
      integer(kind=kind(1)) :: i

      do i = 1,3
         len = 1.0d0/norm_(self%reciprocal_matrix(:,i))
         self%reciprocal_U_matrix(:,i) = self%reciprocal_matrix(:,i)*len
      end do

   end subroutine

!  **************************
!  Geometry altering routines
!  **************************

   subroutine change_from_fractional(self,g)
    type(unitcell_type) :: self
    ! Change the columns of geometry array "g" *from* crystal fractional
    ! coordinates into standard cartesian coordiantes
      real(kind=kind(1.0d0)), dimension(:,:) :: g
      integer(kind=kind(1)) :: n,n_atom

   call ensure_(tonto,size(g,1)==3,"UNITCELL:change_from_fractional ... incorrect dimension for g")
      n_atom = size(g,2)
      do n = 1,n_atom
         call rotate_by_(g(:,n),self%direct_matrix)
      end do

   end subroutine

   subroutine change_into_fractional(self,g)
    type(unitcell_type) :: self
    ! Change the columns of geometry array "g" from standard cartesian
    ! coordinates *into* crystal fractional coordinates
      real(kind=kind(1.0d0)), dimension(:,:) :: g
      integer(kind=kind(1)) :: n,n_atom

   call ensure_(tonto,size(g,1)==3,"UNITCELL:change_into_fractional ... incorrect dimension for g")
      n_atom = size(g,2)
      do n = 1,n_atom
         call rotate_by_(g(:,n),self%inverse_matrix)
      end do

   end subroutine

   subroutine change_from_fractional_1(self,p)
    type(unitcell_type) :: self
    ! Change the position "p" *from* crystal fractional coordinates into standard
    ! cartesian coordiantes
      real(kind=kind(1.0d0)), dimension(3) :: p

      call rotate_by_(p,self%direct_matrix)

   end subroutine

   subroutine change_into_fractional_1(self,p)
    type(unitcell_type) :: self
    ! Change the position "p" from standard cartesian coordinates *into* crystal
    ! fractional coordinates
      real(kind=kind(1.0d0)), dimension(3) :: p

      call rotate_by_(p,self%inverse_matrix)

   end subroutine

!  ************
!  Read methods
!  ************

   recursive subroutine read_keywords(self)
    type(unitcell_type) :: self
    ! Read data from "stdin" using keyword style input.
    ! The following code is inherited from OBJECT
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","UNITCELL:read_keywords ... expecting open bracket symbol, {")
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
    type(unitcell_type) :: self
    ! Process command "keyword". Any required data needed by the "keyword" is
    ! inputted from "stdin".
     character(*) :: keyword
     character(128) :: word

     word = keyword
     call to_lower_case_(word)
     select case (word)
        case ("}                ")   ! exit read_loop
        case ("a=               "); call read_a_(self)
        case ("alpha=           "); call read_alpha_(self)
        case ("angles=          "); call read_angles_(self)
        case ("b=               "); call read_b_(self)
        case ("beta=            "); call read_beta_(self)
        case ("c=               "); call read_c_(self)
        case ("gamma=           "); call read_gamma_(self)
        case ("dimensions=      "); call read_lengths_(self)
        case ("junk=            "); call read_junk_(self)
        case ("lengths=         "); call read_lengths_(self)
        case ("put              "); call put_(self)
        case ("units=           "); call read_units_(self)
        case default;                       allocate(tonto%known_keywords(13))
        tonto%known_keywords(1) = "}                "
        tonto%known_keywords(2) = "a=               "
        tonto%known_keywords(3) = "alpha=           "
        tonto%known_keywords(4) = "angles=          "
        tonto%known_keywords(5) = "b=               "
        tonto%known_keywords(6) = "beta=            "
        tonto%known_keywords(7) = "c=               "
        tonto%known_keywords(8) = "gamma=           "
        tonto%known_keywords(9) = "dimensions=      "
        tonto%known_keywords(10) = "junk=            "
        tonto%known_keywords(11) = "lengths=         "
        tonto%known_keywords(12) = "put              "
        tonto%known_keywords(13) = "units=           "
        call unknown_(tonto,word,"UNITCELL:process_keyword")
        deallocate(tonto%known_keywords)
      end select
     call update_(self)

   end subroutine

   subroutine read_units(self)
    type(unitcell_type) :: self
    ! Read a string which describes the units to be used
    ! The following code is inherited from OBJECT

      call set_default_units_(stdin,next_str_(stdin))

   end subroutine

   subroutine read_junk(self)
    type(unitcell_type) :: self
    ! Read in a junk string, useful for ignoring a field
    ! The following code is inherited from OBJECT

      call skip_next_item_(stdin)

   end subroutine

   subroutine update(self)
    type(unitcell_type) :: self
    ! Update the cell information

     call make_info_(self)

   end subroutine

   subroutine read_lengths(self)
    type(unitcell_type) :: self
    ! Read the unit cell axis lengths

      call read_(stdin,self%length)

   end subroutine

   subroutine read_a(self)
    type(unitcell_type) :: self
    ! Read the a length

      call read_(stdin,self%length(1))

   end subroutine

   subroutine read_b(self)
    type(unitcell_type) :: self
    ! Read the b length

      call read_(stdin,self%length(2))

   end subroutine

   subroutine read_c(self)
    type(unitcell_type) :: self
    ! Read the c length

      call read_(stdin,self%length(3))

   end subroutine

   subroutine read_angles(self)
    type(unitcell_type) :: self
    ! Read the unit cell angles

      call read_(stdin,self%angle)

   end subroutine

   subroutine read_alpha(self)
    type(unitcell_type) :: self
    ! Read the alpha angle

      call read_(stdin,self%angle(1))

   end subroutine

   subroutine read_beta(self)
    type(unitcell_type) :: self
    ! Read the beta angle

      call read_(stdin,self%angle(2))

   end subroutine

   subroutine read_gamma(self)
    type(unitcell_type) :: self
    ! Read the gamma angle

      call read_(stdin,self%angle(3))

   end subroutine

   subroutine read_CIF(self,cif)
    type(unitcell_type) :: self
    ! Read cell information from a type(cif_type) file "cif"
      type(cif_type) :: cif
      real(kind=kind(1.0d0)) :: err
      logical(kind=kind(.true.)) :: found

      call set_defaults_(self)
      self%angle(1) = 90  ! Reset defaults (already set in read_CIF)
      self%angle(2) = 90  ! in degrees !
      self%angle(3) = 90
      call read_item_(cif,"_cell_angle_alpha",self%angle(1),err,found)
      call read_item_(cif,"_cell_angle_beta" ,self%angle(2),err,found)
      call read_item_(cif,"_cell_angle_gamma",self%angle(3),err,found)
      call read_item_(cif,"_cell_length_a",self%length(1),err)
      call read_item_(cif,"_cell_length_b",self%length(2),err)
      call read_item_(cif,"_cell_length_c",self%length(3),err)
      call convert_from_(self%length,"angstrom")
      call convert_from_(self%angle,"degree")     ! Now convert to rads
      call update_(self)

   end subroutine

!  ***********
!  Put methods
!  ***********

   subroutine put(self)
    type(unitcell_type) :: self
    ! Put unitcell information

     call flush_(stdout)
     call text_(stdout,"Unitcell information:")
     call flush_(stdout)
     call show_(stdout,"alpha angle (rad)        = ",self%angle(1))
     call show_(stdout,"beta  angle (rad)        = ",self%angle(2))
     call show_(stdout,"gamma angle (rad)        = ",self%angle(3))
     call show_(stdout,"a cell parameter (bohr)  = ",self%length(1))
     call show_(stdout,"b cell parameter (bohr)  = ",self%length(2))
     call show_(stdout,"c cell parameter (bohr)  = ",self%length(3))
     call show_(stdout,"Cell volume(bohr^3)      = ",self%volume)
     call flush_(stdout)
     call show_(stdout,"alpha* angle (rad)       = ",alpha_star_(self))
     call show_(stdout,"beta*  angle (rad)       = ",beta_star_(self))
     call show_(stdout,"gamma* angle (rad)       = ",gamma_star_(self))
     call show_(stdout,"a* cell parameter (bohr) = ",a_star_(self))
     call show_(stdout,"b* cell parameter (bohr) = ",b_star_(self))
     call show_(stdout,"c* cell parameter (bohr) = ",c_star_(self))
     call flush_(stdout)
     call text_(stdout,"Direct cell matrix/bohr:")
     call put_(stdout,self%direct_matrix)
     call flush_(stdout)
     call text_(stdout,"Inverse direct cell matrix/bohr:")
     call put_(stdout,self%inverse_matrix)
     call flush_(stdout)
     call text_(stdout,"Reciprocal cell matrix/(bohr^{-1}):")
     call put_(stdout,self%reciprocal_matrix)
     call flush_(stdout)
     call text_(stdout,"Direct U cell matrix/bohr:")
     call put_(stdout,self%direct_U_matrix)
     call flush_(stdout)
     call text_(stdout,"Reciprocal U cell matrix/(bohr^{-1}):")
     call put_(stdout,self%reciprocal_U_matrix)
     call flush_(stdout)

   end subroutine

   subroutine put_CX(self,label)
    type(unitcell_type) :: self
    ! Output some information for the Crystal Explorer program.
       character(128) :: label

       call flush_(stdout)
       call text_(stdout,"begin crystalcell " // trim(label))
       call show_(stdout,"   a =",to_units_(self%length(1),"angstrom"))
       call show_(stdout,"   b =",to_units_(self%length(2),"angstrom"))
       call show_(stdout,"   c =",to_units_(self%length(3),"angstrom"))
       call show_(stdout,"   alpha =",to_units_(self%angle(1),"degree"))
       call show_(stdout,"   beta  =",to_units_(self%angle(2),"degree"))
       call show_(stdout,"   gamma =",to_units_(self%angle(3),"degree"))
       call text_(stdout,"end crystalcell")

   end subroutine

end
