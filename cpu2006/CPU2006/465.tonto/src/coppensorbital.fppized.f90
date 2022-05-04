!-------------------------------------------------------------------------------
!
! COPPENSORBITAL: used to describe contracted slater orbitals fitted to
! relativistic Hartree Fock calculations, as described by Coppens and Liu, and
! Macch and Coppens..
!
! Copyright (C) Mark Hore, Dylan Jayatilaka, 2002
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
! $Id: coppensorbital.foo,v 1.2.2.9 2004/04/21 09:12:54 reaper Exp $
!-------------------------------------------------------------------------------
module COPPENSORBITAL_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: same_as_
   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: create_copy_
   use REALVEC_MODULE, only: destroy_

   use INTVEC_MODULE, only: same_as_
   use INTVEC_MODULE, only: create_
   use INTVEC_MODULE, only: create_copy_
   use INTVEC_MODULE, only: destroy_

   use INT_MODULE, only: factorial_

   use STR_MODULE, only: split_
   use STR_MODULE, only: to_lower_case_
   use STR_MODULE, only: includes_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: set_default_units_
   use TEXTFILE_MODULE, only: next_str_
   use TEXTFILE_MODULE, only: skip_next_item_
   use TEXTFILE_MODULE, only: redirect_
   use TEXTFILE_MODULE, only: next_item_
   use TEXTFILE_MODULE, only: show_
   use TEXTFILE_MODULE, only: put_
   use TEXTFILE_MODULE, only: revert_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: reverted_
   use TEXTFILE_MODULE, only: read_ptr_
   use TEXTFILE_MODULE, only: dash_

   use STRVEC_MODULE, only: create_copy_
   use STRVEC_MODULE, only: destroy_
   use STRVEC_MODULE, only: index_of_matching_bracket_

   use REAL_MODULE, only: is_int_
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

   public    values_at_radii_
   interface values_at_radii_
      module procedure values_at_radii
   end interface

   public    read_z_
   interface read_z_
      module procedure read_z
   end interface

   public    read_keys_
   interface read_keys_
      module procedure read_keys
   end interface

   public    read_n_c_z_
   interface read_n_c_z_
      module procedure read_n_c_z
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    read_n_c_z_ptr_
   interface read_n_c_z_ptr_
      module procedure read_n_c_z_ptr
   end interface

   public    same_as_
   interface same_as_
      module procedure same_as
   end interface

   public    read_occupancy_
   interface read_occupancy_
      module procedure read_occupancy
   end interface

   public    read_kind_
   interface read_kind_
      module procedure read_kind
   end interface

   public    value_at_radius_
   interface value_at_radius_
      module procedure value_at_radius
   end interface

   public    put_table_header_
   interface put_table_header_
      module procedure put_table_header
   end interface

   public    read_n_fun_
   interface read_n_fun_
      module procedure read_n_fun
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   public    table_width_
   interface table_width_
      module procedure table_width
   end interface

   public    read_keywords_
   interface read_keywords_
      module procedure read_keywords
   end interface

   public    cc_
   interface cc_
      module procedure cc
   end interface

   public    unnormalise_
   interface unnormalise_
      module procedure unnormalise
   end interface

   public    process_keyword_
   interface process_keyword_
      module procedure process_keyword
   end interface

   public    put_
   interface put_
      module procedure put
   end interface

   public    process_keys_
   interface process_keys_
      module procedure process_keys
   end interface

   public    renormalise_
   interface renormalise_
      module procedure renormalise
   end interface

   public    put_table_footer_
   interface put_table_footer_
      module procedure put_table_footer
   end interface

   public    read_values_cutoff_
   interface read_values_cutoff_
      module procedure read_values_cutoff
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   public    clear_keys_
   interface clear_keys_
      module procedure clear_keys
   end interface

   public    densities_at_radii_
   interface densities_at_radii_
      module procedure densities_at_radii
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    put_table_
   interface put_table_
      module procedure put_table
   end interface

   public    keys_created_
   interface keys_created_
      module procedure keys_created
   end interface

   public    make_values_at_opt_
   interface make_values_at_opt_
      module procedure make_values_at_opt
   end interface

   public    ex_
   interface ex_
      module procedure ex
   end interface

   public    read_c_
   interface read_c_
      module procedure read_c
   end interface

   public    set_keys_
   interface set_keys_
      module procedure set_keys
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    read_junk_
   interface read_junk_
      module procedure read_junk
   end interface

   public    read_kind_occ_num_n_c_z_
   interface read_kind_occ_num_n_c_z_
      module procedure read_kind_occ_num_n_c_z
   end interface

   public    density_at_radius_
   interface density_at_radius_
      module procedure density_at_radius
   end interface

   public    read_n_
   interface read_n_
      module procedure read_n
   end interface

   public    values_at_points_
   interface values_at_points_
      module procedure values_at_points
   end interface

   public    read_units_
   interface read_units_
      module procedure read_units
   end interface

   character(128), dimension(:), pointer, private :: keys => NULL()

   real(kind=kind(1.0d0)), private :: values_cutoff = 10.0d0**(-6)

contains

! ***************************
! Create and destroy routines
! ***************************

   subroutine create(self)
    type(coppensorbital_type) :: self
    ! Create an object
      pointer :: self
    ! The following code is inherited from OBJECT

      nullify(self)
      allocate(self)

      call nullify_ptr_part_(self)
      call set_defaults_(self)

   end subroutine

   subroutine destroy(self)
    type(coppensorbital_type) :: self
    ! Destroy an object
      pointer :: self
    ! The following code is inherited from OBJECT

      if (associated(self)) then
        call destroy_ptr_part_(self)
        deallocate(self)

      end if

   end subroutine

   subroutine nullify_ptr_part(self)
    type(coppensorbital_type) :: self
    ! Nullify the pointer parts of the atomvec

      nullify(self%n)
      nullify(self%c)
      nullify(self%z)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(coppensorbital_type) :: self
    ! Destroy the pointer parts

      call destroy_(self%n)
      call destroy_(self%c)
      call destroy_(self%z)

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
    type(coppensorbital_type) :: self
    ! Set default values

      self%orb_kind = "?"
      self%occupancy = 0.0d0
      self%n_fun = 0

   end subroutine

   subroutine copy(self,c)
    type(coppensorbital_type) :: self
    ! Create a copy of c
       type(coppensorbital_type) :: c

       self = c
       if (associated(c%n)) call create_copy_(self%n,c%n)
       if (associated(c%c)) call create_copy_(self%c,c%c)
       if (associated(c%z)) call create_copy_(self%z,c%z)

   end subroutine

   function ex(self,i) result(res)
    type(coppensorbital_type) :: self
    ! Return the exponent of the "i" th slater orbital
       integer(kind=kind(1)) :: i
       real(kind=kind(1.0d0)) :: res

       res = self%z(i)

   end function

   function cc(self,i) result(res)
    type(coppensorbital_type) :: self
    ! Return the contraction coefficient of the "i" th slater orbital
       integer(kind=kind(1)) :: i
       real(kind=kind(1.0d0)) :: res

       res = self%c(i)

   end function

! ***********
! I/O methods
! ***********

   recursive subroutine read_keywords(self)
    type(coppensorbital_type) :: self
    ! Read data from "stdin" using keyword style input.
    ! The following code is inherited from OBJECT
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","COPPENSORBITAL:read_keywords ... expecting open bracket symbol, {")
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
    type(coppensorbital_type) :: self
    ! Process command "keyword". Data is inputted from "stdin", unless "keyword"
    ! is a sequence of blank separated strings.  In this case, the sequence is
    ! processed as if it were a separate file.
     character(*), intent(in) :: keyword
     character(128) :: word

     word = keyword
     call to_lower_case_(word)
     select case (word)
        case ("}                          ")   ! exit case
        case ("c=                         "); call read_c_(self)
        case ("kind=                      "); call read_kind_(self)
        case ("kind,occupancy,num,n,c,z=  "); call read_kind_occ_num_n_c_z_(self)
        case ("n=                         "); call read_n_(self)
        case ("n,c,z=                     "); call read_n_c_z_(self)
        case ("n,c,z*=                    "); call read_n_c_z_ptr_(self)
        case ("n_fun=                     "); call read_n_fun_(self)
        case ("num=                       "); call read_n_fun_(self)
        case ("occupancy=                 "); call read_occupancy_(self)
        case ("values_cutoff=             "); call read_values_cutoff_(self)
        case ("z=                         "); call read_z_(self)
        case  default;                        allocate(tonto%known_keywords(12))
        tonto%known_keywords(1) = "}                          "
        tonto%known_keywords(2) = "c=                         "
        tonto%known_keywords(3) = "kind=                      "
        tonto%known_keywords(4) = "kind,occupancy,num,n,c,z=  "
        tonto%known_keywords(5) = "n=                         "
        tonto%known_keywords(6) = "n,c,z=                     "
        tonto%known_keywords(7) = "n,c,z*=                    "
        tonto%known_keywords(8) = "n_fun=                     "
        tonto%known_keywords(9) = "num=                       "
        tonto%known_keywords(10) = "occupancy=                 "
        tonto%known_keywords(11) = "values_cutoff=             "
        tonto%known_keywords(12) = "z=                         "
        call unknown_(tonto,word,"COPPENSORBITAL:process_keyword")
        deallocate(tonto%known_keywords)
     end select

   end subroutine

   subroutine read_units(self)
    type(coppensorbital_type) :: self
    ! Read a string which describes the units to be used
    ! The following code is inherited from OBJECT

      call set_default_units_(stdin,next_str_(stdin))

   end subroutine

   subroutine read_junk(self)
    type(coppensorbital_type) :: self
    ! Read in a junk string, useful for ignoring a field
    ! The following code is inherited from OBJECT

      call skip_next_item_(stdin)

   end subroutine

   subroutine read_kind(self)
    type(coppensorbital_type) :: self
    ! Read in the orbital kind ("1s", "2s", "2p" ....)

      call read_(stdin,self%orb_kind)

   end subroutine

   subroutine read_occupancy(self)
    type(coppensorbital_type) :: self
    ! Read in the orbital occupancy

      call read_(stdin,self%occupancy)

   end subroutine

   subroutine read_n_fun(self)
    type(coppensorbital_type) :: self
    ! Read in the number of contracted functions.

      call read_(stdin,self%n_fun)

   end subroutine

   subroutine read_n(self)
    type(coppensorbital_type) :: self
    ! Read in the "n" quantum numbers. NOTE: n_fun must already have been input.
      call ensure_(tonto,self%n_fun>0,"COPPENSORBITAL:read_n ... n_fun is negative; use n_fun= before this command")

      call destroy_(self%n)
      call create_(self%n,self%n_fun)
      call read_(stdin,self%n)

   end subroutine

   subroutine read_c(self)
    type(coppensorbital_type) :: self
    ! Read in the "c" contraction coefficients. NOTE: n_fun must already have
    ! been input.
      call ensure_(tonto,self%n_fun>0,"COPPENSORBITAL:read_c ... n_fun is negative; use n_fun= before this command")

      call destroy_(self%c)
      call create_(self%c,self%n_fun)
      call read_(stdin,self%c)

   end subroutine

   subroutine read_z(self)
    type(coppensorbital_type) :: self
    ! Read in the "z" slater function exponents. NOTE: n_fun must already have
    ! been input.
      call ensure_(tonto,self%n_fun>0,"COPPENSORBITAL:read_z ... n_fun is negative; use n_fun= before this command")

      call destroy_(self%z)
      call create_(self%z,self%n_fun)
      call read_(stdin,self%z)

   end subroutine

   subroutine read_n_c_z(self)
    type(coppensorbital_type) :: self
    ! Read in the "n", "c" and "z" vectors. NOTE: .n_fun must previously have been inputted.
      integer(kind=kind(1)) :: i
      real(kind=kind(1.0d0)) :: val
      call ensure_(tonto,self%n_fun>0,"COPPENSORBITAL:read_n_c_z ... n_fun is negative; use n_fun= before this command")

      call destroy_(self%n); call destroy_(self%c); call destroy_(self%z)
      call create_(self%n,self%n_fun)
      call create_(self%c,self%n_fun)
      call create_(self%z,self%n_fun)
      do i = 1,self%n_fun
         call read_(stdin,val)
         call ensure_(tonto,is_int_(val),"COPPENSORBITAL:read_n_c_z ... n number is not integer")
         self%n(i) = val
         call read_(stdin,self%c(i))
         call read_(stdin,self%z(i))
      end do

   end subroutine

   subroutine read_n_c_z_ptr(self)
    type(coppensorbital_type) :: self
    ! Read in the "n", "c" and "z" vectors. NOTE: it is not necessary to read in
    ! n_fun, it will be defined from the vector length.
      real(kind=kind(1.0d0)), dimension(:), pointer :: tmp
      integer(kind=kind(1)) :: i,k

      call read_(stdin,tmp)
      call ensure_(tonto,mod(size(tmp),3)==0,"COPPENSORBITAL:read_n_c_z_ptr ... number of data not divisible by three")
      self%n_fun = size(tmp)/3
      call destroy_(self%n); call destroy_(self%c); call destroy_(self%z)
      call create_(self%n,self%n_fun)
      call create_(self%c,self%n_fun)
      call create_(self%z,self%n_fun)
      k = 1
      do i = 1,3
         call ensure_(tonto,is_int_(tmp(k)),"COPPENSORBITAL:read_n_c_z_ptr ... n number is not integer")
         self%n(k) = tmp(k)
         self%c(k) = tmp(k+1)
         self%z(k) = tmp(k+2)
         k = k + 3
      end do
      call destroy_(tmp)

   end subroutine

   subroutine read_kind_occ_num_n_c_z(self)
    type(coppensorbital_type) :: self
    ! Read in everything: the "kind", "occupnacy", "n_fun", and "n", "c" and "z".

      call read_kind_(self)
      call read_occupancy_(self)
      call read_n_fun_(self)
      call read_n_c_z_(self)

   end subroutine

   subroutine read_values_cutoff(self)
    type(coppensorbital_type) :: self
    ! Read in a cutoff below which values of the the coppensorbital on a grid are
    ! set to zero.

      call read_(stdin,values_cutoff)

   end subroutine

!  ********************
!  Key related routines
!  ********************

   subroutine read_keys(self)
    type(coppensorbital_type) :: self
    ! Read the "keys".
    ! The following code is inherited from OBJECT

     call clear_keys_(self)
     call read_ptr_(stdin,keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)

   end subroutine

   subroutine process_keys(self)
    type(coppensorbital_type) :: self
    ! Process each of the words in the "keys" list.
    ! The following code is inherited from OBJECT
      integer(kind=kind(1)) :: k,l,n_key
      character(128) :: keyword
      character(128), dimension(:), pointer :: internal

      call ensure_(tonto,associated(keys),"COPPENSORBITAL:process_keys ... no keys")
      n_key = size(keys)
      k = 0
      do
         k = k + 1
         keyword = keys(k)
         if (keyword=="}") exit
         if (keyword=="{") then
            l = index_of_matching_bracket_(keys(k:),"{")
            call ensure_(tonto,l>0,"COPPENSORBITAL:process_keys ... no matching closing brace, }")
            internal => keys(k:k+l-1)
            call redirect_(stdin,internal)
            call read_keywords_(self)
            call revert_(stdin)
            k = k+l-1
         else if (includes_(keyword," ")) then
            internal => split_(keyword)
            call redirect_(stdin,internal)
            call read_keywords_(self)
            call destroy_(internal)
            call revert_(stdin)
         else
            call process_keyword_(self,keyword)
         end if
         if (k==n_key) exit
      end do

   end subroutine

   function keys_created(self) result(res)
    type(coppensorbital_type) :: self
    ! Return .true. if the list-element keys are created.
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from OBJECT

      res = associated(keys)

   end function

   subroutine set_keys(self,the_keys)
    type(coppensorbital_type) :: self
    ! This is for setting the "keys" externally.
     character(len=*), dimension(:) :: the_keys
    ! The following code is inherited from OBJECT

     call clear_keys_(self)
     call create_copy_(keys,the_keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)

   end subroutine

   subroutine clear_keys(self)
    type(coppensorbital_type) :: self
    ! This is for destroying the "keys" externally.
    ! The following code is inherited from OBJECT

     if (associated(keys)) then
       call destroy_(keys)
       call ignore_memory_leak_(tonto,memory_blocks_gained=-1)
     end if

   end subroutine

   subroutine put_table_footer(self)
    type(coppensorbital_type) :: self
    ! Output a table footer from the list of "keys". NOTE: not all keywords need
    ! contribute to the banner - any unrecognised keyword is skipped.
    ! The following code is inherited from OBJECT

     call dash_(stdout,width=table_width_(self))

   end subroutine

   subroutine put_table_header(self)
    type(coppensorbital_type) :: self
    ! Output a table header from the list of "keys". NOTE: not all keywords need
    ! contribute to the banner - any unrecognised keyword is skipped.
     character(128) :: word
     integer(kind=kind(1)) :: width,k

     width = table_width_(self)
     if (width > 0) then
       call dash_(stdout,width=width)
       do k = 1,size(keys)
         word = keys(k)
         call to_lower_case_(word)
         select case (word)
           case ("flush        "); call flush_(stdout); exit
           case ("put_kind     "); call put_(stdout,"kind",int_width=.true.)
           case ("put_occ      "); call put_(stdout,"Occ.",int_width=.true.)
           case ("put_occupancy"); call put_(stdout,"Occ.",int_width=.true.)
           case ("put_n_fun    "); call put_(stdout,"n_fun",int_width=.true.)
           case default
         end select
         if (k==size(keys)) then
           call flush_(stdout)  ! In case they didn't write one.
           call warn_(tonto,"COPPENSORBITAL:put_table_header ... no flush keyword - you may later overrun the output buffer")
         end if
       end do
       call dash_(stdout,width=width)
     end if

   end subroutine

   function table_width(self) result(res)
    type(coppensorbital_type) :: self
    ! Return how wide a table is, based on "keys".  Note that not all keywords
    ! need to contribute to the banner - any unrecognised keyword is skipped.
     integer(kind=kind(1)) :: res
     character(128) :: word
     integer(kind=kind(1)) :: int_dash,real_dash,k

     int_dash = 0
     real_dash = 0
     call ensure_(tonto,associated(keys),"COPPENSORBITAL:table_width ... no keywords")
     do k = 1,size(keys)
       word = keys(k)
       call to_lower_case_(word)
       select case (word)
         case ("put_kind     "); int_dash = int_dash + 1
         case ("put_occ      "); int_dash = int_dash + 1
         case ("put_occupancy"); int_dash = int_dash + 1
         case ("put_n_fun    "); int_dash = int_dash + 1
         case ("flush        "); exit
         case default
       end select
     end do
     res = int_dash * stdout%int_width + real_dash * stdout%real_width

   end function

! *******
! Methods
! *******

   function same_as(self,b) result(res)
    type(coppensorbital_type) :: self
    ! Return .true. if "self" is the same as "b".
      intent(in) :: self
      type(coppensorbital_type), intent(in) :: b
      logical(kind=kind(.true.)) :: res

      res = self%orb_kind == b%orb_kind .and. self%occupancy == b%occupancy &
            .and. same_as_(self%n,b%n) .and. same_as_(self%c,b%c) .and. same_as_(self%z,b%z)

   end function

   function density_at_radius(self,R) result(res)
    type(coppensorbital_type) :: self
    ! Returns the value of the coppens orbital at radius "R".
      real(kind=kind(1.0d0)), intent(in) :: R
      real(kind=kind(1.0d0)) :: res

      res = value_at_radius_(self,R)
      res = self%occupancy*res*res

   end function

   function densities_at_radii(self,R) result(res)
    type(coppensorbital_type) :: self
    ! Returns the values of the coppens density at all the radii "R".
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: R
      real(kind=kind(1.0d0)), dimension(size(R)) :: res

      res = values_at_radii_(self,R)
      res = self%occupancy*res*res

   end function

   function value_at_radius(self,R) result(res)
    type(coppensorbital_type) :: self
    ! Returns the value of the coppens orbital at radius "R".
      real(kind=kind(1.0d0)), intent(in) :: R
      real(kind=kind(1.0d0)) :: res
      integer(kind=kind(1)), dimension(:), pointer :: nm1

      res = 0.0d0
      call create_(nm1,size(self%n))
      nm1 = self%n - 1
      res = sum(self%c * R**nm1 * exp(-self%z*R))
      call destroy_(nm1)

   end function

   function values_at_radii(self,R) result(res)
    type(coppensorbital_type) :: self
    ! Returns the values of the coppens orbital at all the radii "R".
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: R
      real(kind=kind(1.0d0)), dimension(size(R)) :: res
      integer(kind=kind(1)), dimension(:), pointer :: nm1
      integer(kind=kind(1)) :: i

      call create_(nm1,size(self%n))
      nm1 = self%n - 1
      do i = 1,size(R)
         res(i) = sum(self%c * R(i)**nm1 * exp(-self%z*R(i)) )
      end do
      call destroy_(nm1)

   end function

   subroutine unnormalise(self)
    type(coppensorbital_type) :: self
    ! Set the value of the coefficient to correspond to un-normalised Slater
    ! functions -- assuming they are normalised. This saves in computation time.
      integer(kind=kind(1)) :: i,n2

      do i = 1,self%n_fun
        n2 = 2*self%n(i)
        self%c(i) = self%c(i) * sqrt(2.0d0*self%z(i)/factorial_(n2)) * (2.0d0*self%z(i))**self%n(i)
      end do

   end subroutine

   subroutine renormalise(self)
    type(coppensorbital_type) :: self
    ! Set the value of the coefficient to correspond to normalised slater
    ! functions --- assuming they are w.r.t. unnormalised functions.
      integer(kind=kind(1)) :: i,n2

      do i = 1,self%n_fun
        n2 = 2*self%n(i)
        self%c(i) = self%c(i) / (sqrt(2.0d0*self%z(i)/factorial_(n2)) * (2.0d0*self%z(i))**self%n(i))
      end do

   end subroutine

   function values_at_points(self,pt,pos) result(res)
    type(coppensorbital_type) :: self
    ! Make the orbital values on the series of points specified in "pt", assuming
    ! the orbital is at point "pos"; and put the results in "res".
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos
      real(kind=kind(1.0d0)), dimension(size(pt,1)) :: res
      real(kind=kind(1.0d0)), dimension(:), pointer :: R
      real(kind=kind(1.0d0)) :: x,y,z
      integer(kind=kind(1)) :: n,n_pt

      call ensure_(tonto,size(pt,2)==3,"COPPENSORBITAL:values_at_points ... incorrect second dimension, pt array")
      n_pt = size(pt,1)
      call create_(R,n_pt)
      do n = 1,n_pt
         x = pt(n,1) - pos(1)
         y = pt(n,2) - pos(2)
         z = pt(n,3) - pos(3)
         R(n) = sqrt(x*x + y*y + z*z)
      end do
      res = values_at_radii_(self,R)
      call destroy_(R)

   end function

   subroutine make_values_at_opt(self,pt,pos,grid)
    type(coppensorbital_type) :: self
    ! Make the orbital values on the series of points specified in "pt", assuming
    ! the orbital is at point "pos"; and put the results in the array "grid".
    ! This is an optimised version.
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos
      real(kind=kind(1.0d0)), dimension(:), intent(out) :: grid
      real(kind=kind(1.0d0)), dimension(:), pointer :: tmp,n_val
      real(kind=kind(1.0d0)) :: x,y,z,R,R2,pos1,pos2,pos3
      real(kind=kind(1.0d0)) :: cutoff,cutoff1,cutoff2,cutoff3,cutoff4,minz,maxn1divminz
      integer(kind=kind(1)) :: n,n_pt,i,n2,maxn1

      call ensure_(tonto,size(grid)==size(pt,1),"COPPENSORBITAL:make_values_at_opt ... grid size wrong!")

       ! The cutoff of the orbital value.  Values smaller than this are set to
       ! zero.
      cutoff = values_cutoff
      call create_(n_val,size(self%n))
      n_val = self%n - 1.0d0

      pos1 = pos(1)
      pos2 = pos(2)
      pos3 = pos(3)
      call create_(tmp,self%n_fun)
      do i = 1,self%n_fun
        n2 = 2*self%n(i)
        tmp(i) = self%c(i) * sqrt(2.0d0*self%z(i)/factorial_(n2)) * (2.0d0*self%z(i))**self%n(i)
      end do

      maxn1 = maxval(self%n)-1
      minz =  minval(self%z)
      maxn1divminz = maxn1/minz

       ! cutoff1 <= maxn1*log(R) - minz*R.  (Strict test).
      cutoff1 = log(cutoff/(self%n_fun*maxval(tmp)))
      cutoff2 = cutoff1/minz

       ! Looser yet strict test.
       ! log(R) < R, substitute into strict test.
      cutoff3 = cutoff1/(maxn1-minz)
       ! Do the test on R^2, so avoid sqrt below where possible.
      cutoff4 = cutoff3*cutoff3

      n_pt = size(pt,1)
      do n = 1,n_pt
         x = pt(n,1) - pos1
         y = pt(n,2) - pos2
         z = pt(n,3) - pos3
         R2 = x*x + y*y + z*z

          ! Do maxn==1 as special case.
         if (maxn1==0) then

            ! Do test using R2.
           if (cutoff4 < R2) then
             grid(n) = 0.0d0
           else if (R2 == 0.0d0) then
             grid(n) = sum(0.0d0**n_val(:) * tmp(:))
           else
              ! Do test using R.
             R = sqrt(R2)
             if (cutoff2 > -R) then
               grid(n) = 0.0d0
             else
               grid(n) = sum(R**n_val(:) * exp(-self%z(:)*R) * tmp(:))
             end if
           end if

          ! not maxn==1 special case.
         else

            ! Do test using R2.
           if (cutoff4 < R2) then
             grid(n) = 0.0d0
           else if (R2 == 0.0d0) then
             grid(n) = sum(tmp(:))
           else
              ! Do test using R.
             R = sqrt(R2)
             if (cutoff2 > maxn1divminz*log(R)-R) then
               grid(n) = 0.0d0
             else
               grid(n) = sum(R**n_val(:) * exp(-self%z(:)*R) * tmp(:))
             end if
           end if
         end if

      end do
      call destroy_(tmp)
      call destroy_(n_val)

   end subroutine

   subroutine put(self)
    type(coppensorbital_type) :: self
    ! Put the orbital information to "stdout"

      call flush_(stdout)
      call show_(stdout,"Kind        = ",self%orb_kind)
      call show_(stdout,"Occupancy   = ",self%occupancy)
      call show_(stdout,"No. of funs = ",self%n_fun)
      call put_table_(self)

   end subroutine

   subroutine put_table(self)
    type(coppensorbital_type) :: self
    ! Put the orbital information to "stdout" in table format
      integer(kind=kind(1)) :: i

      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=2)
      call put_(stdout,"#",int_width=.true.)
      call put_(stdout,"N",int_width=.true.)
      call put_(stdout,"Exponent")
      call put_(stdout,"Coeff.")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=2)
      do i = 1,self%n_fun
         call put_(stdout,i)
         call put_(stdout,self%n(i))
         call put_(stdout,self%z(i))
         call put_(stdout,self%c(i))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=2,real_fields=2)

   end subroutine

end
