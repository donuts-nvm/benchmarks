!-------------------------------------------------------------------------------
!
! For describing a single gaussian function :: GAUSSIAN.
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
! $Id: gaussian.foo,v 1.7.2.1 2003/03/06 10:40:56 dylan Exp $
!-------------------------------------------------------------------------------

module GAUSSIAN_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use INTVEC_MODULE, only: create_
   use INTVEC_MODULE, only: destroy_

   use STR_MODULE, only: is_int_
   use STR_MODULE, only: to_lower_case_
   use STR_MODULE, only: to_int_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: set_default_units_
   use TEXTFILE_MODULE, only: next_str_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: skip_next_item_
   use TEXTFILE_MODULE, only: move_to_previous_item_
   use TEXTFILE_MODULE, only: reverted_
   use TEXTFILE_MODULE, only: next_item_
   use TEXTFILE_MODULE, only: show_

   use INT_MODULE, only: to_str_
   use INT_MODULE, only: make_gaussian_xyz_powers_

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

   public    set_
   interface set_
      module procedure set
      module procedure set_1
      module procedure set_2
   end interface

   public    read_l_chr_
   interface read_l_chr_
      module procedure read_l_chr
   end interface

   public    read_l_int_
   interface read_l_int_
      module procedure read_l_int
   end interface

   public    create_
   interface create_
      module procedure create
      module procedure create_1
      module procedure create_2
   end interface

   public    read_pos_
   interface read_pos_
      module procedure read_pos
   end interface

   public    n_comp_
   interface n_comp_
      module procedure n_comp
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    read_ex_
   interface read_ex_
      module procedure read_ex
   end interface

   public    make_grid_
   interface make_grid_
      module procedure make_grid
      module procedure make_grid_1
   end interface

   public    ex_
   interface ex_
      module procedure ex
   end interface

   public    pos_
   interface pos_
      module procedure pos
      module procedure pos_1
   end interface

   public    read_keywords_
   interface read_keywords_
      module procedure read_keywords
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    l_
   interface l_
      module procedure l
   end interface

   public    read_junk_
   interface read_junk_
      module procedure read_junk
   end interface

   public    read_l_
   interface read_l_
      module procedure read_l
   end interface

   public    l_chr_
   interface l_chr_
      module procedure l_chr
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

   public    copy_
   interface copy_
      module procedure copy
   end interface

contains

   subroutine create(self)
    type(gaussian_type) :: self
    ! Create a gaussian object
      pointer :: self

      nullify(self)
      allocate(self)

   end subroutine

   subroutine create_1(self,l,pos,ex)
    type(gaussian_type) :: self
    ! Create a gaussian object
      pointer :: self
       integer(kind=kind(1)) :: l
      real(kind=kind(1.0d0)), dimension(3) :: pos
      real(kind=kind(1.0d0)) :: ex

      nullify(self)
      allocate(self)

      call set_(self,l,pos,ex)

   end subroutine

   subroutine create_2(self,l,Rx,Ry,Rz,ex)
    type(gaussian_type) :: self
    ! Create a gaussian object
      pointer :: self
       integer(kind=kind(1)) :: l
      real(kind=kind(1.0d0)) :: Rx,Ry,Rz
      real(kind=kind(1.0d0)) :: ex

      nullify(self)
      allocate(self)

      call set_(self,l,Rx,Ry,Rz,ex)

   end subroutine

   subroutine destroy(self)
    type(gaussian_type) :: self
    ! Destroy a shell object
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

   subroutine create_copy(self,G)
    type(gaussian_type) :: self
    ! Create a copy of "G"
      pointer :: self
       type(gaussian_type) :: G

      call create_(self)
      call copy_(self,G)

   end subroutine

   subroutine copy(self,G)
    type(gaussian_type) :: self
    ! Set the gaussian to "G"
      type(gaussian_type) :: G

      self = G

   end subroutine

   subroutine set(self,l,pos,ex)
    type(gaussian_type) :: self
    ! Set a gaussian object
       integer(kind=kind(1)) :: l
      real(kind=kind(1.0d0)), dimension(3) :: pos
      real(kind=kind(1.0d0)) :: ex

      self%l = l
      self%pos = pos
      self%ex = ex

   end subroutine

   subroutine set_1(self,l,Rx,Ry,Rz,ex)
    type(gaussian_type) :: self
    ! Create a gaussian object
       integer(kind=kind(1)) :: l
      real(kind=kind(1.0d0)) :: Rx,Ry,Rz
      real(kind=kind(1.0d0)) :: ex

      self%l = l
      self%pos = (/Rx,Ry,Rz/)
      self%ex = ex

   end subroutine

   subroutine set_2(self,ex)
    type(gaussian_type) :: self
    ! Set a gaussian object
      real(kind=kind(1.0d0)) :: ex

      self%ex = ex

   end subroutine

   function l_chr(self) result(res)
    type(gaussian_type) :: self
    ! Return a character representation for the angular mtm
      character(1) :: res
       integer(kind=kind(1)) :: l

      l = self%l
      select case (l)
         case (0); res="s"
         case (1); res="p"
         case (2); res="d"
         case (3); res="f"
         case (4); res="g"
         case default;
            call ensure_(tonto,l<=23,"GAUSSIAN:l_chr ... angular momentum too large:"// trim(to_str_(l)))
            res = achar(l-4+iachar("g"))
      end select

   end function

!  **************
!  Output methods
!  **************

   subroutine put(self)
    type(gaussian_type) :: self
    ! Put the gaussian information to file "stdout"

      call show_(stdout,"L quantum number = ",self%l,real_width=.true.)
      call show_(stdout,"Position         = ",self%pos)
      call show_(stdout,"Exponent         = ",self%ex)

   end subroutine

!  *************
!  Input methods
!  *************

   recursive subroutine read_keywords(self)
    type(gaussian_type) :: self
    ! Read data from "stdin" using keyword style input.
    ! The following code is inherited from OBJECT
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","GAUSSIAN:read_keywords ... expecting open bracket symbol, {")
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
    type(gaussian_type) :: self
    ! Process command "keyword". Any required data needed by the "keyword" is
    ! inputted from "stdin".
      character(*) :: keyword
      character(128) :: word

      word = keyword
      call to_lower_case_(word)
      select case (word)
         case ("}     ");    ! exit case
         case ("ex=   ");   call read_ex_(self)
         case ("l=    ");   call read_l_(self)
         case ("l_chr=");   call read_l_chr_(self)
         case ("l_int=");   call read_l_int_(self)
         case ("pos=  ");   call read_pos_(self)
         case ("put   ");   call put_(self)
         case ("units=");   call read_units_(self)
         case default;      allocate(tonto%known_keywords(8))
         tonto%known_keywords(1) = "}     "
         tonto%known_keywords(2) = "ex=   "
         tonto%known_keywords(3) = "l=    "
         tonto%known_keywords(4) = "l_chr="
         tonto%known_keywords(5) = "l_int="
         tonto%known_keywords(6) = "pos=  "
         tonto%known_keywords(7) = "put   "
         tonto%known_keywords(8) = "units="
         call unknown_(tonto,word,"GAUSSIAN:process_keyword")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_units(self)
    type(gaussian_type) :: self
    ! Read a string which describes the units to be used
    ! The following code is inherited from OBJECT

      call set_default_units_(stdin,next_str_(stdin))

   end subroutine

   subroutine read_junk(self)
    type(gaussian_type) :: self
    ! Read in a junk string, useful for ignoring a field
    ! The following code is inherited from OBJECT

      call skip_next_item_(stdin)

   end subroutine

   subroutine read_l(self)
    type(gaussian_type) :: self
    ! Read in the l symbol
      character(128) :: word

      call read_(stdin,word)
      call move_to_previous_item_(stdin)
      if (is_int_(word)) then; call read_l_int_(self)
      else;                  call read_l_chr_(self)
      end if

   end subroutine

   subroutine read_l_int(self)
    type(gaussian_type) :: self
    ! Read in the l integer
      character(128) :: word

      call read_(stdin,word)
      call ensure_(tonto,is_int_(word),"GAUSSIAN:read_l_int ... expecting an integer for L")
      self%l = to_int_(word)

   end subroutine

   subroutine read_l_chr(self)
    type(gaussian_type) :: self
    ! Read in the l symbol
      character(128) :: word
      character(1) :: l_c
      integer(kind=kind(1)) :: l

      call read_(stdin,word)
      call ensure_(tonto,len_trim(word)==1,"GAUSSIAN:read_l_chr ... unknown L symbol")
      l_c = word
      call to_lower_case_(l_c)
      select case (l_c)
         case ("s"); l = 0
         case ("p"); l = 1
         case ("d"); l = 2
         case ("f"); l = 3
         case ("g"); l = 4
         case default;
            call die_if_(tonto,l_c<"g","GAUSSIAN:read_l_chr ... unknown angular momentum character: "//l_c)
            l = 4 + iachar(l_c)-iachar("g")
      end select
      self%l = l

   end subroutine

   subroutine read_ex(self)
    type(gaussian_type) :: self
    ! Read in the exponents

      call read_(stdin,self%ex)

   end subroutine

   subroutine read_pos(self)
    type(gaussian_type) :: self
    ! Read in the position

      call read_(stdin,self%pos)

   end subroutine

!*****************
!  Inquiry methods
!*****************

   pure function l(self) result(res)
    type(gaussian_type) :: self
    ! Return the angular momentum l
      intent(in) :: self
      integer(kind=kind(1)) :: res
      res = self%l

   end function

   pure function n_comp(self) result(res)
    type(gaussian_type) :: self
    ! Return the number of components in the gaussian shell.
      intent(in) :: self
      integer(kind=kind(1)) :: res
      res = (self%l+1)*(self%l+2)/2

   end function

   pure function pos(self) result(res)
    type(gaussian_type) :: self
    ! Return the position of the shell
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(3) :: res
      res = self%pos

   end function

   pure function pos_1(self,comp) result(res)
    type(gaussian_type) :: self
    ! Return the position of the shell
      intent(in) :: self
      integer(kind=kind(1)), intent(in) :: comp
      real(kind=kind(1.0d0)) :: res
      res = self%pos(comp)

   end function

   pure function ex(self) result(res)
    type(gaussian_type) :: self
    ! Return the exponent vector pointer
      intent(in) :: self
      real(kind=kind(1.0d0)) :: res
      res = self%ex

   end function

   subroutine make_grid(self,g,pt)
    type(gaussian_type) :: self
    ! Return "g(i,n)", the value of the gaussian component "n" on grid point "i"
    ! given a set of grid points "pt(i,1:3)"
      real(kind=kind(1.0d0)), dimension(:,:), target :: pt
       real(kind=kind(1.0d0)), dimension(:,:), target :: g
      real(kind=kind(1.0d0)), dimension(:), pointer :: x,y,z

   call ensure_(tonto,size(g,2)==n_comp_(self),"GAUSSIAN:make_grid ... incorrectly dimensioned")
      x => pt(:,1); y => pt(:,2); z => pt(:,3)
      call make_grid_(self,g,x,y,z)

   end subroutine

   subroutine make_grid_1(self,g,x,y,z)
    type(gaussian_type) :: self
    ! Return "g(i,n)", the value of the gaussian component "n" on grid point "i"
    ! given a set of grid points "(x(i),y(i),z(i))"
      real(kind=kind(1.0d0)), dimension(:) :: x,y,z
      real(kind=kind(1.0d0)), dimension(:,:) :: g
      integer(kind=kind(1)), dimension(:), pointer :: nx,ny,nz
      integer(kind=kind(1)) :: n_pt,n,b,j
      real(kind=kind(1.0d0)) :: rr,xx,yy,zz,posx,posy,posz,g0,gbn

      call ensure_(tonto,size(g,2)==n_comp_(self),"GAUSSIAN:make_grid_1 ... incorrectly dimensioned")
      n_pt = size(x)
      posx = self%pos(1); posy = self%pos(2); posz = self%pos(3)
      select case (self%l)
        case (0)
          do n = 1,n_pt
             xx=x(n) - posx; yy=y(n) - posy; zz=z(n) - posz
             rr = xx*xx + yy*yy + zz*zz
             g(n,1) = exp( -rr * self%ex )
          end do
        case (1)
          do n = 1,n_pt
             xx=x(n) - posx; yy=y(n) - posy; zz=z(n) - posz
             rr = xx*xx + yy*yy + zz*zz
             g0 = exp( -rr * self%ex )
             g(n,1) = xx*g0
             g(n,2) = yy*g0
             g(n,3) = zz*g0
          end do
        case (2)
          do n = 1,n_pt
             xx=x(n) - posx; yy=y(n) - posy; zz=z(n) - posz
             rr = xx*xx + yy*yy + zz*zz
             g0 = exp( -rr * self%ex )
             g(n,1) = xx*xx*g0
             g(n,2) = yy*yy*g0
             g(n,3) = zz*zz*g0
             g(n,4) = xx*yy*g0
             g(n,5) = xx*zz*g0
             g(n,6) = yy*zz*g0
          end do
        case default
          call create_(nx,n_comp_(self)); call create_(ny,n_comp_(self)); call create_(nz,n_comp_(self))
          call make_gaussian_xyz_powers_(self%l,nx,ny,nz)
          do n = 1,n_pt
             xx=x(n) - posx; yy=y(n) - posy; zz=z(n) - posz
             rr = xx*xx + yy*yy + zz*zz     ! Do exponential part of the gaussian
             g0 = exp( -rr * self%ex )
             do b = 1,n_comp_(self)               ! Loop over all basis functions
                gbn = g0                    ! Combine exponential and cartesian
                do j = 1,nx(b)              ! power of the x^j part
                   gbn = xx*gbn
                end do
                do j = 1,ny(b)
                   gbn = yy*gbn
                end do
                do j = 1,nz(b)
                   gbn = zz*gbn
                end do
                g(n,b) = gbn
             end do
          end do
          call destroy_(nz); call destroy_(ny); call destroy_(nx)
     end select

   end subroutine

end
