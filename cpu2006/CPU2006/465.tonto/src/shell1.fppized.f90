!-------------------------------------------------------------------------------
!
! SHELL1: For describing contracted cartesian gaussian shells with a position
! coordinate
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
! $Id: shell1.foo,v 1.27.2.1 2003/03/06 10:40:57 dylan Exp $
!
!-------------------------------------------------------------------------------

module SHELL1_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: same_as_
   use REALVEC_MODULE, only: normalising_factors_
   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: create_copy_
   use REALVEC_MODULE, only: destroy_

   use INT_MODULE, only: double_factorial_
   use INT_MODULE, only: to_str_
   use INT_MODULE, only: make_gaussian_xyz_powers_

   use INTMAT_MODULE, only: create_
   use INTMAT_MODULE, only: destroy_

   use INTVEC_MODULE, only: create_
   use INTVEC_MODULE, only: destroy_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: set_default_units_
   use TEXTFILE_MODULE, only: next_str_
   use TEXTFILE_MODULE, only: skip_next_item_
   use TEXTFILE_MODULE, only: redirect_
   use TEXTFILE_MODULE, only: show_
   use TEXTFILE_MODULE, only: put_
   use TEXTFILE_MODULE, only: revert_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: reverted_
   use TEXTFILE_MODULE, only: move_to_previous_item_
   use TEXTFILE_MODULE, only: dash_

   use STR_MODULE, only: is_int_
   use STR_MODULE, only: to_lower_case_
   use STR_MODULE, only: to_int_
   use STR_MODULE, only: includes_

   use REALMAT_MODULE, only: create_
   use REALMAT_MODULE, only: destroy_
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

   public    set_
   interface set_
      module procedure set
      module procedure set_1
   end interface

   public    read_l_chr_
   interface read_l_chr_
      module procedure read_l_chr
   end interface

   public    read_l_int_
   interface read_l_int_
      module procedure read_l_int
   end interface

   public    update_
   interface update_
      module procedure update
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    make_nabla_grid_
   interface make_nabla_grid_
      module procedure make_nabla_grid
      module procedure make_nabla_grid_1
   end interface

   public    norm_
   interface norm_
      module procedure norm
   end interface

   public    create_
   interface create_
      module procedure create
      module procedure create_1
   end interface

   public    read_pos_
   interface read_pos_
      module procedure read_pos
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
      module procedure create_copy_1
   end interface

   public    make_laplacian_grid_
   interface make_laplacian_grid_
      module procedure make_laplacian_grid
      module procedure make_laplacian_grid_1
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

   public    same_as_
   interface same_as_
      module procedure same_as
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   public    read_keywords_
   interface read_keywords_
      module procedure read_keywords
   end interface

   public    read_junk_ex_cc_
   interface read_junk_ex_cc_
      module procedure read_junk_ex_cc
   end interface

   public    read_cc_
   interface read_cc_
      module procedure read_cc
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    make_nabla_grid_fdm_
   interface make_nabla_grid_fdm_
      module procedure make_nabla_grid_fdm
   end interface

   public    read_l_
   interface read_l_
      module procedure read_l
   end interface

   public    l_chr_
   interface l_chr_
      module procedure l_chr
   end interface

   public    unnormalise_
   interface unnormalise_
      module procedure unnormalise
   end interface

   public    process_keyword_
   interface process_keyword_
      module procedure process_keyword
   end interface

   public    read_units_
   interface read_units_
      module procedure read_units
   end interface

   public    n_prim_
   interface n_prim_
      module procedure n_prim
   end interface

   public    put_
   interface put_
      module procedure put
   end interface

   public    renormalise_
   interface renormalise_
      module procedure renormalise
   end interface

   public    make_contraction_matrix_
   interface make_contraction_matrix_
      module procedure make_contraction_matrix
   end interface

   public    read_ex_cc_
   interface read_ex_cc_
      module procedure read_ex_cc
   end interface

   public    read_n_cc_
   interface read_n_cc_
      module procedure read_n_cc
   end interface

   public    copy_
   interface copy_
      module procedure copy
      module procedure copy_1
   end interface

contains

   subroutine create(self)
    type(shell1_type) :: self
    ! Create a shell object
      pointer :: self

      nullify(self)
      allocate(self)

      call nullify_ptr_part_(self)
      call set_defaults_(self)

   end subroutine

   subroutine create_1(self,shell)
    type(shell1_type) :: self
    ! Create a shell object from another copy
      pointer :: self
      type(shell1_type), intent(in) :: shell

      call create_(self)
      call copy_(self,shell)

   end subroutine

   subroutine create_copy(self,shell)
    type(shell1_type) :: self
    ! Create a copy of a shell1 object
      pointer :: self
      type(shell1_type), intent(in) :: shell

      call create_(self)
      call copy_(self,shell)

   end subroutine

   subroutine create_copy_1(self,shell,pos)
    type(shell1_type) :: self
    ! Create a shell object from another copy
      pointer :: self
      type(shell_type), intent(in) :: shell
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: pos

      call create_(self)
      call copy_(self,shell,pos)

   end subroutine

   subroutine destroy(self)
    type(shell1_type) :: self
    ! Destroy a shell object
      pointer :: self

      if (.not. associated(self)) then;   return; end if
      call destroy_ptr_part_(self)

      deallocate(self)

   end subroutine

   subroutine nullify_ptr_part(self)
    type(shell1_type) :: self
    ! Nullify the pointer parts of self

     nullify(self%ex)
     nullify(self%cc)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(shell1_type) :: self
    ! Destroy pointer part of a shell object

      call destroy_(self%ex)
      call destroy_(self%cc)

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

   subroutine copy(self,shell)
    type(shell1_type) :: self
    ! Make a shell1 object from a shell1 object.
    ! NOTE : ensure the ptr parts have been destroyed or nullified beforehand.
     type(shell1_type), intent(in) :: shell

     self = shell
     call create_copy_(self%ex,shell%ex)
     call create_copy_(self%cc,shell%cc)

   end subroutine

   subroutine copy_1(self,shell,pos)
    type(shell1_type) :: self
    ! Make a shell1 object from the shell and its position.
    ! NOTE : ensure the ptr parts have been destroyed beforehand.
     intent(out) :: self
     type(shell_type), intent(in) :: shell
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: pos

     call ensure_(tonto,.not. associated(self%ex),"SHELL1:copy_1 ... ex not destroyed")
     call ensure_(tonto,.not. associated(self%cc),"SHELL1:copy_1 ... cc not destroyed")
     self%n_cc = shell%n_cc
     self%l = shell%l
     self%n_comp = shell%n_comp
     self%pos = pos
     call create_copy_(self%ex,shell%ex)
     call create_copy_(self%cc,shell%cc)

   end subroutine

   subroutine set(self,shell,pos)
    type(shell1_type) :: self
    ! Set a shell1 object.
      real(kind=kind(1.0d0)), dimension(:), optional :: pos
      type(shell_type), optional :: shell

      if (present(pos)) self%pos = pos
      if (present(shell)) then
         self%l = shell%l
         self%n_comp = shell%n_comp
         self%n_cc = shell%n_cc
         self%ex => shell%ex
         self%cc => shell%cc
      end if

   end subroutine

   subroutine set_1(self,shell)
    type(shell1_type) :: self
    ! Set a shell1 object
      type(shell1_type) :: shell

      self = shell

   end subroutine

   subroutine set_defaults(self)
    type(shell1_type) :: self
    !

      self%l = 0
      self%n_comp = 0
      self%n_cc = 0
      self%pos = 0.0d0

   end subroutine

   function n_prim(self) result(res)
    type(shell1_type) :: self
    ! Return the number of primitive gaussians in the shell
      integer(kind=kind(1)) :: res

      res = self%n_comp*self%n_cc

   end function

   subroutine update(self)
    type(shell1_type) :: self
    ! Update the shell data

      self%n_comp = (self%l+1)*(self%l+2)/2

   end subroutine

!   n_comp_sum result (res)
!   ! No. of cartesian components up to shell with momentum .l
!     res :: integer(kind=kind(1))
!     res = (.l+1)*(.l+2)*(.l+3)/6
!   end

   subroutine put(self)
    type(shell1_type) :: self
    ! Put the shell information to "stdout"
       integer(kind=kind(1)) :: i

      call flush_(stdout)
      call show_(stdout,"L quantum number =",self%l,real_width=.true.)
      call show_(stdout,"Position         =",self%pos)
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=2)
      call put_(stdout,"N", int_width=.true.)
      call put_(stdout,"Exponents")
      call put_(stdout,"Contraction")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=2)
      do i = 1,self%n_cc
         call put_(stdout,i)
         call put_(stdout, self%ex(i))
         call put_(stdout, self%cc(i))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=2)

   end subroutine

!  *************
!  Input methods
!  *************

   recursive subroutine read_keywords(self)
    type(shell1_type) :: self
    ! Read data from "stdin" using keyword style input.
      character(128) :: word

      call read_(stdin,word)
      call die_if_(tonto,word/="{","SHELL1:read_keywords ... expecting an open bracket symbol, {")
      read_loop: do              ! Loop over keywords
         call read_(stdin,word)
         if (word=="}")         exit read_loop
         if (reverted_(stdin))    exit read_loop
         call process_keyword_(self,word)
      end do read_loop
      call update_(self)

   end subroutine

   subroutine process_keyword(self,keyword)
    type(shell1_type) :: self
    ! Process a command "keyword". Data is inputted from "stdin", unless
    ! "word" is a sequence of blank separated strings. In this case,
    ! the sequence is processed as if it were a separate file.
      character(*) :: keyword
      character(128) :: word

      word = keyword
      call to_lower_case_(word)
      if (includes_(word," ")) then
         call redirect_(stdin,(/word/))
         call read_keywords_(self)
         call revert_(stdin)
      else
         select case (word)
           case ("}          ");  ! exit case
           case ("cc=        "); call read_cc_(self)
           case ("ex=        "); call read_ex_(self)
           case ("ex,cc=     "); call read_ex_cc_(self)
           case ("junk,ex,cc="); call read_junk_ex_cc_(self)
           case ("l=         "); call read_l_(self)
           case ("l_chr=     "); call read_l_chr_(self)
           case ("l_int=     "); call read_l_int_(self)
           case ("n_cc=      "); call read_n_cc_(self)
           case ("pos=       "); call read_pos_(self)
           case ("put        "); call put_(self)
           case ("units=     "); call read_units_(self)
           case  default ;       allocate(tonto%known_keywords(12))
           tonto%known_keywords(1) = "}          "
           tonto%known_keywords(2) = "cc=        "
           tonto%known_keywords(3) = "ex=        "
           tonto%known_keywords(4) = "ex,cc=     "
           tonto%known_keywords(5) = "junk,ex,cc="
           tonto%known_keywords(6) = "l=         "
           tonto%known_keywords(7) = "l_chr=     "
           tonto%known_keywords(8) = "l_int=     "
           tonto%known_keywords(9) = "n_cc=      "
           tonto%known_keywords(10) = "pos=       "
           tonto%known_keywords(11) = "put        "
           tonto%known_keywords(12) = "units=     "
           call unknown_(tonto,word,"SHELL1:process_keyword")
           deallocate(tonto%known_keywords)
         end select
      end if

   end subroutine

   subroutine read_units(self)
    type(shell1_type) :: self
    ! Read a string which describes the units to be used

      call set_default_units_(stdin,next_str_(stdin))

   end subroutine

   subroutine read_l(self)
    type(shell1_type) :: self
    ! Read in the l symbol
      character(128) :: word

      call read_(stdin,word)
      call move_to_previous_item_(stdin)
      if (is_int_(word)) then; call read_l_int_(self)
      else;                  call read_l_chr_(self)
      end if

   end subroutine

   subroutine read_l_int(self)
    type(shell1_type) :: self
    ! Read in the l integer
      character(128) :: word

      call read_(stdin,word)
      call ensure_(tonto,is_int_(word),"SHELL1:read_l_int ... expecting an integer for L")
      self%l = to_int_(word)

   end subroutine

   subroutine read_l_chr(self)
    type(shell1_type) :: self
    ! Read in the l symbol
      character(128) :: word
      character(1) :: l_c
      integer(kind=kind(1)) :: l

      call read_(stdin,word)
      call ensure_(tonto,len_trim(word)==1,"SHELL1:read_l_chr ... unknown L symbol")
      l_c = word
      call to_lower_case_(l_c)
      select case (l_c)
         case ("s"); l = 0
         case ("p"); l = 1
         case ("d"); l = 2
         case ("f"); l = 3
         case ("g"); l = 4
         case default;
            call die_if_(tonto,l_c<"g","SHELL1:read_l_chr ... unknown angular momentum character: "//l_c)
            l = 4 + iachar(l_c)-iachar("g")
      end select
      self%l = l

   end subroutine

   subroutine read_pos(self)
    type(shell1_type) :: self
    ! Read in the position

      call read_(stdin,self%pos)

   end subroutine

   subroutine read_n_cc(self)
    type(shell1_type) :: self
    ! Read in the number of contraction coefficients

      call read_(stdin,self%n_cc)
   call ensure_(tonto,self%n_cc>0,"SHELL1:read_n_cc ... n_cc must be positive")

   end subroutine

   subroutine read_ex(self)
    type(shell1_type) :: self
    ! Read in the exponents

      call ensure_(tonto,self%n_cc>0,"SHELL1:read_ex ... n_cc must be entered first")
      call ensure_(tonto,.not. associated(self%ex),"SHELL1:read_ex ... ex already entered")
      call create_(self%ex,self%n_cc)
      call read_(stdin,self%ex)

   end subroutine

   subroutine read_cc(self)
    type(shell1_type) :: self
    ! Read in the contraction coefficients

      call ensure_(tonto,self%n_cc>0,"SHELL1:read_cc ... n_cc must be entered first")
      call ensure_(tonto,.not. associated(self%cc),"SHELL1:read_cc ... cc already entered")
      call create_(self%cc,self%n_cc)
      call read_(stdin,self%cc)

   end subroutine

   subroutine read_ex_cc(self)
    type(shell1_type) :: self
    ! Read in the exponents and contractions

      call ensure_(tonto,self%n_cc>0,"SHELL1:read_ex_cc ... n_cc must be entered first")
      call ensure_(tonto,.not. associated(self%ex),"SHELL1:read_ex_cc ... ex already entered")
      call ensure_(tonto,.not. associated(self%cc),"SHELL1:read_ex_cc ... ex already entered")
      call create_(self%ex,self%n_cc)
      call create_(self%cc,self%n_cc)
      call read_(stdin,self%ex,self%cc)

   end subroutine

   subroutine read_junk_ex_cc(self)
    type(shell1_type) :: self
    ! Read in the exponents and contractions preceded by a junk string
      integer(kind=kind(1)) :: i

      call ensure_(tonto,self%n_cc>0,"SHELL1:read_junk_ex_cc ... n_cc must be entered first")
      call ensure_(tonto,.not. associated(self%ex),"SHELL1:read_junk_ex_cc ... ex already entered")
      call ensure_(tonto,.not. associated(self%cc),"SHELL1:read_junk_ex_cc ... ex already entered")
      call create_(self%ex,self%n_cc)
      call create_(self%cc,self%n_cc)
      do i = 1,self%n_cc
         call skip_next_item_(stdin)
         call read_(stdin,self%ex(i))
         call read_(stdin,self%cc(i))
      end do

   end subroutine

   function same_as(self,sh) result(same)
    type(shell1_type) :: self
    ! Return .true. if the shell "self" is the same as "sh".
      type(shell1_type) :: sh
      logical(kind=kind(.true.)) :: same

      same = self%l==sh%l .and. self%n_comp==sh%n_comp &
         .and. same_as_(self%ex,sh%ex) .and. same_as_(self%cc,sh%cc) &
         .and. same_as_(self%pos,sh%pos)

   end function

   function l_chr(self) result(res)
    type(shell1_type) :: self
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
            call die_if_(tonto,l>23,"SHELL1:l_chr ... angular momentum too large:"// trim(to_str_(l)))
            res = achar(l-4+iachar("g"))
      end select

   end function

   subroutine make_contraction_matrix(self,ccm)
    type(shell1_type) :: self
    ! Return the contraction coefficient matrix. Size of ccm is (.n_prim,.n_comp).
      real(kind=kind(1.0d0)), dimension(:,:) :: ccm
      integer(kind=kind(1)) :: b,p,i
      real(kind=kind(1.0d0)) :: f
      real(kind=kind(1.0d0)), dimension(:), pointer :: fac

      call create_(fac, self%n_comp )
      call normalising_factors_(fac, self%l )
      do b = 1,self%n_comp        ! do over basis components
         p = b                ! primitive p
         f = fac(b)
         do i = 1,self%n_cc       ! do over contractions
            ccm(p,b) = f*self%cc(i)
            p = p + self%n_comp
         end do
      end do
      call destroy_(fac)

   end subroutine

   function norm(self) result(res)
    type(shell1_type) :: self
    ! Return the norm of the shell, assuming that the existing contraction
    ! coefficients are with respect to NORMALISED gaussians
      real(kind=kind(1.0d0)) :: res
      integer(kind=kind(1)) :: i,j
      real(kind=kind(1.0d0)) :: sum,a,b,ab

      sum = 0.0d0
      do i = 1,self%n_cc
         a = self%ex(i)
         do j = 1,i-1
            b = self%ex(j)
            ab = 2.0d0*sqrt(a*b)/(a+b);
            sum = sum + 2.0d0*self%cc(i)*self%cc(j)*ab**(self%l+1.0d0+0.50d0);
         end do
         sum = sum + self%cc(i)*self%cc(i)
      end do
      res = sqrt(sum) * ((2.0d0*3.141592653589793d0)**(3.0d0/4.0d0))

   end function

   subroutine unnormalise(self)
    type(shell1_type) :: self
    ! Unnormalise self as if all components in the shell were x^l, and also
    ! assuming the existing contraction coefficients are initially with respect
    ! to NORMALISED gaussians. It is up to you to correct this factor with
    ! appropriate double factorial square roots for other components.

       ! Take out the normalisation of each primitive.
       ! The double factorial should be: fac = 1/sqrt(df(nx)*df(ny)*df(nz))
       ! where n are the cartesian powers of the basis component

       self%cc(:) = self%cc(:) * (4.0d0*self%ex(:))**(0.50d0*self%l+0.50d0+0.25d0) &
                * (1.0d0/(norm_(self)*sqrt(double_factorial_(self%l))))

   end subroutine

   subroutine renormalise(self)
    type(shell1_type) :: self
    ! Normalise self as if all components in the shell were x^l, and also
    ! assuming the existing contraction coefficients are with respect to raw
    ! unnormalised gaussians. This will undo routine "unnormalise".
    ! The overall ((2.0d0*3.141592653589793d0)**(3.0d0/4.0d0)) / .norm factor is to make the
    ! cc=1 for a shell with one primitive

      self%cc(:) = self%cc(:) / ((4.0d0*self%ex(:))**(0.50d0*self%l+0.50d0+0.25d0)) &
               * (sqrt(double_factorial_(self%l)) * ((2.0d0*3.141592653589793d0)**(3.0d0/4.0d0)) / norm_(self))

   end subroutine

   subroutine make_grid(self,g,pt)
    type(shell1_type) :: self
    ! Return "g(i,n)", the value of the shell component "n" on grid point "i"
    ! given a set of grid points "pt(i,1:3)"
    ! Note: it is assumed that the shell is normalised, and the appropriate
    ! double factorial normalising factors are introduced for each component
      real(kind=kind(1.0d0)), dimension(:,:), target :: pt
       real(kind=kind(1.0d0)), dimension(:,:), target :: g
      real(kind=kind(1.0d0)), dimension(:), pointer :: x,y,z

      x => pt(:,1); y => pt(:,2); z => pt(:,3)
      call make_grid_(self,g,x,y,z)

   end subroutine

   subroutine make_grid_1(self,f,x,y,z)
    type(shell1_type) :: self
    ! Return "f(i,n)", the value of the shell component "n" on grid point "i"
    ! given a set of grid points "(x(i),y(i),z(i))"
    ! Note: it is assumed that the shell is normalised; the appropriate
    ! double factorial normalising factors are introduced for each component
      real(kind=kind(1.0d0)), dimension(:) :: x,y,z
      real(kind=kind(1.0d0)), dimension(:,:) :: f
      real(kind=kind(1.0d0)), dimension(:), pointer :: fac,bx,by,bz
      integer(kind=kind(1)), dimension(:), pointer :: nx,ny,nz
      integer(kind=kind(1)) :: n_pt,n,b
      real(kind=kind(1.0d0)) :: rr,xx,yy,zz,posx,posy,posz,f0,f1,bxb,byb,bzb
      real(kind=kind(1.0d0)) :: xx2,yy2,zz2,xx3,yy3,zz3,zz_f1

      n_pt = size(x)
      posx = self%pos(1); posy = self%pos(2); posz = self%pos(3)
      select case (self%l)
        case (0)
          do n = 1,n_pt
             xx=x(n) - posx; yy=y(n) - posy; zz=z(n) - posz
             rr = xx*xx + yy*yy + zz*zz
             f(n,1) = sum( self%cc * exp( -rr * self%ex ))
          end do
        case (1)
          do n = 1,n_pt
             xx=x(n) - posx; yy=y(n) - posy; zz=z(n) - posz

              ! Radial part, summed over primitives
             rr = xx*xx + yy*yy + zz*zz
             f0 = sum( self%cc * exp( -rr * self%ex ))

             f(n,1) = xx*f0
             f(n,2) = yy*f0
             f(n,3) = zz*f0
          end do
        case (2)
          do n = 1,n_pt
             xx=x(n) - posx; yy=y(n) - posy; zz=z(n) - posz

              ! Radial part, summed over primitives
             rr = xx*xx + yy*yy + zz*zz
             f0 = sum( self%cc * exp( -rr * self%ex ))

             f1 = sqrt(3.0d0)*f0
             zz_f1 = zz*f1
             f(n,1) = xx*xx*f0
             f(n,2) = yy*yy*f0
             f(n,3) = zz*zz*f0
             f(n,4) = xx*yy*f1
             f(n,5) = xx*zz_f1
             f(n,6) = yy*zz_f1
          end do
        case (3)
          call create_(fac, self%n_comp );    call normalising_factors_(fac, self%l )
          do n = 1,n_pt
             xx=x(n) - posx; yy=y(n) - posy; zz=z(n) - posz
             xx2 = xx*xx
             yy2 = yy*yy
             zz2 = zz*zz
             xx3 = xx2*xx
             yy3 = yy2*yy
             zz3 = zz2*zz

              ! Radial part, summed over primitives
             rr = xx2 + yy2 + zz2
             f0 = sum( self%cc * exp( -rr * self%ex ))

             f(n,1)  = fac(1)*f0*xx3
             f(n,2)  = fac(2)*f0*yy3
             f(n,3)  = fac(3)*f0*zz3
             f(n,4)  = fac(4)*f0*xx2*yy
             f(n,5)  = fac(5)*f0*xx2*zz
             f(n,6)  = fac(6)*f0*xx*yy2
             f(n,7)  = fac(7)*f0*yy2*zz
             f(n,8)  = fac(8)*f0*xx*zz2
             f(n,9)  = fac(9)*f0*yy*zz2
             f(n,10) = fac(10)*f0*xx*yy*zz
          end do
          call destroy_(fac)
        case default
          call create_(nx,self%n_comp); call create_(ny,self%n_comp); call create_(nz,self%n_comp)
          call make_gaussian_xyz_powers_(self%l,nx,ny,nz)
          call create_(fac, self%n_comp );    call normalising_factors_(fac, self%l )
          call create_(bx,0,self%l)
          call create_(by,0,self%l)
          call create_(bz,0,self%l)
          do n = 1,n_pt
             xx=x(n) - posx; yy=y(n) - posy; zz=z(n) - posz

              ! Radial part, summed over primitives
             rr = xx*xx + yy*yy + zz*zz
             f0 = sum( self%cc * exp( -rr * self%ex ))

              ! Cartesian orbital part
              ! x**b,y**b,z**b for b=0,.l
             bx(0) = 1.0d0;   by(0) = 1.0d0;   bz(0) = 1.0d0
             bx(1) = xx;    by(1) = yy;    bz(1) = zz
             bxb = xx;      byb = yy;      bzb = zz
             do b=2,self%l
               bxb = bxb*xx;   byb = byb*yy;   bzb = bzb*zz
               bx(b) = bxb;    by(b) = byb;    bz(b) = bzb
             end do

              ! Combine cartesian orbital powers with the exponential part
             f(n,:) = fac(:)*f0*bx(nx(:))*by(ny(:))*bz(nz(:))
          end do
          call destroy_(bz)
          call destroy_(by)
          call destroy_(bx)
          call destroy_(fac)
          call destroy_(nz); call destroy_(ny); call destroy_(nx)
     end select

   end subroutine

   subroutine make_nabla_grid(self,g,pt)
    type(shell1_type) :: self
    ! Return "g(i,n,1:3)", the value of the gradient of the shell component "n"
    ! on grid point "i" given a set of grid points "pt(i,1:3)".
      real(kind=kind(1.0d0)), dimension(:,:) :: pt
      real(kind=kind(1.0d0)), dimension(:,:,:) :: g
      real(kind=kind(1.0d0)), dimension(:), pointer :: fac,cc_exp_rr,bx,by,bz,gxb,gyb,gzb
      integer(kind=kind(1)), dimension(:,:), pointer :: nn
      integer(kind=kind(1)) :: n_pt,n,b,nxb,nyb,nzb
      real(kind=kind(1.0d0)) :: xx,yy,zz,rr,facb
      real(kind=kind(1.0d0)) :: g0,g1,g1x,g1y,g1z,x,y,z,bxb,byb,bzb

      call ensure_(tonto,size(pt,2)==3,"SHELL1:make_nabla_grid ... pt matrix incorrectly dimensioned")
      n_pt = size(pt,1)
      x = self%pos(1);  y = self%pos(2);  z = self%pos(3)
      select case (self%l)
        case (0)
          do n = 1,n_pt               ! Do the exponential part of the gaussian
             xx=pt(n,1) - x;   yy=pt(n,2) - y;   zz=pt(n,3) - z
             rr = xx*xx + yy*yy + zz*zz
             g1 = -2.0d0*sum(self%ex*self%cc*exp(-self%ex*rr))
             g(n,1,1) = g1*xx
             g(n,1,2) = g1*yy
             g(n,1,3) = g1*zz
          end do
        case (1)
          call create_(cc_exp_rr,self%n_cc)
          do n = 1,n_pt               ! Do the exponential part of the gaussian
             xx=pt(n,1) - x;   yy=pt(n,2) - y;   zz=pt(n,3) - z
             rr = xx*xx + yy*yy + zz*zz
             cc_exp_rr = self%cc*exp(-self%ex*rr)
             g0 = sum(cc_exp_rr)
             g1 = -2.0d0*sum(self%ex*cc_exp_rr)
             g1x = g1*xx
             g1y = g1*yy
             g1z = g1*zz
             g(n,1,1) = g0+xx*g1x
             g(n,1,2) = xx*g1y
             g(n,1,3) = xx*g1z
             g(n,2,1) = yy*g1x
             g(n,2,2) = g0+yy*g1y
             g(n,2,3) = yy*g1z
             g(n,3,1) = zz*g1x
             g(n,3,2) = zz*g1y
             g(n,3,3) = g0+zz*g1z
          end do
          call destroy_(cc_exp_rr)
        case default
          call create_(cc_exp_rr,self%n_cc)
          call create_(nn,3, self%n_comp );   call make_gaussian_xyz_powers_(self%l,nn)
          call create_(fac, self%n_comp );    call normalising_factors_(fac, self%l )
          call create_(bx,0,self%l);   call create_(by,0,self%l);   call create_(bz,0,self%l)
          call create_(gxb,0,self%l);  call create_(gyb,0,self%l);  call create_(gzb,0,self%l)
          do n = 1,n_pt               ! Do the exponential part of the gaussian
             xx=pt(n,1) - x;   yy=pt(n,2) - y;   zz=pt(n,3) - z
             rr = xx*xx + yy*yy + zz*zz
             cc_exp_rr = self%cc*exp(-self%ex*rr)
             g0 = sum(cc_exp_rr)
             g1 = -2.0d0*sum(self%ex*cc_exp_rr)
             g1x = g1*xx
             g1y = g1*yy
             g1z = g1*zz
              ! gxb(b)=x**(b-1),  bx(b)=x**b for b=0,.l
             bx(0) = 1.0d0;         by(0) = 1.0d0;         bz(0) = 1.0d0
             bx(1) = xx;          by(1) = yy;          bz(1) = zz
             bxb = xx;            byb = yy;            bzb = zz
             gxb(0) = g1x;        gyb(0) = g1y;        gzb(0) = g1z
             gxb(1) = g0+xx*g1x;  gyb(1) = g0+yy*g1y;  gzb(1) = g0+zz*g1z
             do b=2,self%l
               gxb(b) = (b*g0+xx*g1x) * bxb            ! (n-1)th power
               gyb(b) = (b*g0+yy*g1y) * byb            ! of the xyz part
               gzb(b) = (b*g0+zz*g1z) * bzb
               bxb = bxb*xx;   byb = byb*yy;   bzb = bzb*zz
               bx(b) = bxb;    by(b) = byb;    bz(b) = bzb
             end do
              ! Loop over basis functions
             do b = 1,self%n_comp
                nxb = nn(1,b)
                nyb = nn(2,b)
                nzb = nn(3,b)
                facb = fac(b)          ! Basis fn. normalisation factor
                g(n,b,1) = facb * gxb(nxb) *  by(nyb) *  bz(nzb)
                g(n,b,2) = facb *  bx(nxb) * gyb(nyb) *  bz(nzb)
                g(n,b,3) = facb *  bx(nxb) *  by(nyb) * gzb(nzb)
             end do
          end do
          call destroy_(gzb);  call destroy_(gyb);  call destroy_(gxb)
          call destroy_(bz);   call destroy_(by);   call destroy_(bx)
          call destroy_(fac)
          call destroy_(nn)
          call destroy_(cc_exp_rr)
      end select

   end subroutine

   subroutine make_nabla_grid_1(self,g,f,pt)
    type(shell1_type) :: self
    ! Return "g(i,n,1:3)", the value of the gradient of the shell component "n"
    ! on grid point "i" given a set of grid points "pt(i,1:3)".
      real(kind=kind(1.0d0)), dimension(:,:) :: f,pt
      real(kind=kind(1.0d0)), dimension(:,:,:) :: g
      real(kind=kind(1.0d0)), dimension(:), pointer :: fac,cc_exp_rr,bx,by,bz,gxb,gyb,gzb
      integer(kind=kind(1)), dimension(:,:), pointer :: nn
      integer(kind=kind(1)) :: n_pt,n,b,nxb,nyb,nzb
      real(kind=kind(1.0d0)) :: xx,yy,zz,rr,facb,xx_g1x,yy_g1y,zz_g1z
      real(kind=kind(1.0d0)) :: g0,g1,g1x,g1y,g1z,x,y,z,bxb,byb,bzb,by_bz,facb_bx

      call ensure_(tonto,size(pt,2)==3,"SHELL1:make_nabla_grid_1 ... pt matrix incorrectly dimensioned")
      n_pt = size(pt,1)
      x = self%pos(1);  y = self%pos(2);  z = self%pos(3)
      call create_(cc_exp_rr,self%n_cc)
      select case (self%l)
        case (0)
          do n = 1,n_pt               ! Do the exponential part of the gaussian
             xx=pt(n,1) - x;   yy=pt(n,2) - y;   zz=pt(n,3) - z

              ! Radial part, summed over primitives
             rr = xx*xx + yy*yy + zz*zz
             cc_exp_rr = self%cc*exp(-self%ex*rr)
             g1 = -2.0d0*sum(self%ex*cc_exp_rr)

             f(n,1)   = sum(cc_exp_rr)
             g(n,1,1) = g1*xx
             g(n,1,2) = g1*yy
             g(n,1,3) = g1*zz
          end do
        case (1)
          do n = 1,n_pt               ! Do the exponential part of the gaussian
             xx=pt(n,1) - x;   yy=pt(n,2) - y;   zz=pt(n,3) - z

              ! Radial part, summed over primitives
             rr = xx*xx + yy*yy + zz*zz
             cc_exp_rr = self%cc*exp(-self%ex*rr)
             g0 = sum(cc_exp_rr)
             g1 = -2.0d0*sum(self%ex*cc_exp_rr)

             g1x = g1*xx
             g1y = g1*yy
             g1z = g1*zz
             f(n,1) = xx*g0
             f(n,2) = yy*g0
             f(n,3) = zz*g0
             g(n,1,1) = g0+xx*g1x
             g(n,1,2) = xx*g1y
             g(n,1,3) = xx*g1z
             g(n,2,1) = yy*g1x
             g(n,2,2) = g0+yy*g1y
             g(n,2,3) = yy*g1z
             g(n,3,1) = zz*g1x
             g(n,3,2) = zz*g1y
             g(n,3,3) = g0+zz*g1z
          end do
        case default
          call create_(nn,3, self%n_comp );   call make_gaussian_xyz_powers_(self%l,nn)
          call create_(fac, self%n_comp );    call normalising_factors_(fac, self%l )
          call create_(bx,0,self%l);   call create_(by,0,self%l);   call create_(bz,0,self%l)
          call create_(gxb,0,self%l);  call create_(gyb,0,self%l);  call create_(gzb,0,self%l)
          do n = 1,n_pt               ! Do the exponential part of the gaussian
             xx=pt(n,1) - x;   yy=pt(n,2) - y;   zz=pt(n,3) - z
              ! Radial part, summed over primitives
             rr = xx*xx + yy*yy + zz*zz
             cc_exp_rr = self%cc*exp(-self%ex*rr)
             g0 = sum(cc_exp_rr)
             g1 = -2.0d0*sum(self%ex*cc_exp_rr)
              ! Cartesian orbital part
              ! gxb(b)=x**(b-1),  bx(b)=x**b for b=0,.l
             g1x = g1*xx
             g1y = g1*yy
             g1z = g1*zz
             xx_g1x = xx*g1x
             yy_g1y = yy*g1y
             zz_g1z = zz*g1z
             bxb = xx;            byb = yy;            bzb = zz
             bx(0) = 1.0d0;         by(0) = 1.0d0;         bz(0) = 1.0d0
             bx(1) = xx;          by(1) = yy;          bz(1) = zz
             gxb(0) = g1x;        gyb(0) = g1y;        gzb(0) = g1z
             gxb(1) = g0+xx_g1x;  gyb(1) = g0+yy_g1y;  gzb(1) = g0+zz_g1z
             do b=2,self%l
               gxb(b) = (b*g0+xx_g1x) * bxb           ! (n-1)th power
               gyb(b) = (b*g0+yy_g1y) * byb           ! of the xyz part
               gzb(b) = (b*g0+zz_g1z) * bzb
               bxb = bxb*xx;   byb = byb*yy;   bzb = bzb*zz
               bx(b) = bxb;    by(b) = byb;    bz(b) = bzb
             end do
              ! Combine cartesian orbital powers with the exponential part
             do b = 1,self%n_comp
               nxb = nn(1,b)
               nyb = nn(2,b)
               nzb = nn(3,b)
               facb = fac(b)          ! Basis fn. normalisation factor
               bxb = bx(nxb)
               byb = by(nyb)
               bzb = bz(nzb)
               by_bz = byb *  bzb
               facb_bx = facb  *  bxb
               f(n,b)   = facb_bx * by_bz * g0
               g(n,b,1) = facb    * gxb(nxb) * by_bz
               g(n,b,2) = facb_bx * gyb(nyb) * bzb
               g(n,b,3) = facb_bx * byb      * gzb(nzb)
             end do

          end do
          call destroy_(gzb);  call destroy_(gyb);  call destroy_(gxb)
          call destroy_(bz);   call destroy_(by);   call destroy_(bx)
          call destroy_(fac)
          call destroy_(nn)
      end select
      call destroy_(cc_exp_rr)

   end subroutine

   subroutine make_nabla_grid_fdm(self,g,f,pt)
    type(shell1_type) :: self
    ! Return "g(i,n,1:3)", the value of the gradient of the shell component "n"
    ! on grid point "i" given a set of grid points "pt(i,1:3)".
    ! This nabla grid is produced using the finite difference method.  It is much
    ! slower, but useful for checking.
      real(kind=kind(1.0d0)), dimension(:,:) :: f,pt
      real(kind=kind(1.0d0)), dimension(:,:,:) :: g
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: f1,f2,pt1
      real(kind=kind(1.0d0)), dimension(:), pointer :: comp
      real(kind=kind(1.0d0)) :: alpha
      integer(kind=kind(1)) :: n_pt,i

      call ensure_(tonto,size(pt,2)==3,"SHELL1:make_nabla_grid_fdm ... pt matrix incorrectly dimensioned")
      n_pt = size(pt,1)
      call make_grid_(self,f,pt)
      alpha = 10.0d0**(-6)
      call create_(pt1,n_pt,3)
      call create_(f1,n_pt,self%n_comp)
      call create_(f2,n_pt,self%n_comp)
      do i=1,3
        comp => pt1(:,i)
        pt1 = pt
        comp = comp + alpha
        call make_grid_(self,f1,pt1)
        pt1 = pt
        comp = comp - alpha
        call make_grid_(self,f2,pt1)
        g(:,:,i) = 0.50d0/alpha * (f2(:,:) - f1(:,:))
      end do
      call destroy_(f2)
      call destroy_(f1)
      call destroy_(pt1)

   end subroutine

   subroutine make_laplacian_grid(self,g,pt)
    type(shell1_type) :: self
    ! Return "g(i,n,1:3)", the value of the second derivatives
    ! (d/dx2,d/dy2,d/dz2) of the shell component "n" on grid point "i" given a
    ! set of grid points "pt(i,1:3)".
      real(kind=kind(1.0d0)), dimension(:,:) :: pt
      real(kind=kind(1.0d0)), dimension(:,:,:) :: g
      real(kind=kind(1.0d0)), dimension(:), pointer :: fac
      integer(kind=kind(1)), dimension(:,:), pointer :: nn
      integer(kind=kind(1)) :: n_pt,n,b,p,j,nx,ny,nz
      real(kind=kind(1.0d0)) :: aa,a2,x,y,z,rr,val
      real(kind=kind(1.0d0)) :: g0,g1,g2,g2x,g2y,g2z
      real(kind=kind(1.0d0)) :: gnbx,gnby,gnbz,x2,y2,z2
      real(kind=kind(1.0d0)) :: gx0,gx1,gy0,gy1,gz0,gz1,facb
      real(kind=kind(1.0d0)) :: tn,tn2

      call ensure_(tonto,size(pt,2)==3,"SHELL1:make_laplacian_grid ... pt matrix incorrectly dimensioned")
      n_pt = size(pt,1)
      select case (self%l)
        case (0)
          do n = 1,n_pt              ! Do the exponential part of the gaussian
            x=pt(n,1) - self%pos(1); y=pt(n,2) - self%pos(2); z=pt(n,3) - self%pos(3)
            x2 = x*x;   y2 = y*y;   z2 = z*z
            rr = x2 + y2 + z2
            g1 = 0.0d0
            g2 = 0.0d0
            do p = 1,self%n_cc           ! Loop over primitives
              aa = self%ex(p)
              a2 = aa+aa
              val = self%cc(p)*exp(-aa*rr)
              g1 = g1 - a2*val
              g2 = g2 + a2*a2*val
            end do
            g(n,1,1) = g1 + g2*x2
            g(n,1,2) = g1 + g2*y2
            g(n,1,3) = g1 + g2*z2
          end do
        case (1)
          do n = 1,n_pt              ! Do the exponential part of the gaussian
            x=pt(n,1) - self%pos(1); y=pt(n,2) - self%pos(2); z=pt(n,3) - self%pos(3)
            x2 = x*x;   y2 = y*y;   z2 = z*z
            rr = x2 + y2 + z2
            g1 = 0.0d0
            g2 = 0.0d0
            do p = 1,self%n_cc           ! Loop over primitives
              aa = self%ex(p)
              a2 = aa+aa
              val = self%cc(p)*exp(-aa*rr)
              g1 = g1 - a2*val
              g2 = g2 + a2*a2*val
            end do
            g2x = g2 * x2
            g2y = g2 * y2
            g2z = g2 * z2
            g(n,1,1) = x * (3 * g1 + g2x)
            g(n,1,2) = x * (g1 + g2y)
            g(n,1,3) = x * (g1 + g2z)
            g(n,2,1) = y * (g1 + g2x)
            g(n,2,2) = y * (3 * g1 + g2y)
            g(n,2,3) = y * (g1 + g2z)
            g(n,3,1) = z * (g1 + g2x)
            g(n,3,2) = z * (g1 + g2y)
            g(n,3,3) = z * (3 * g1 + g2z)
          end do
        case default
          call create_(nn,3,self%n_comp);   call make_gaussian_xyz_powers_(self%l,nn)
          call create_(fac,self%n_comp);    call normalising_factors_(fac,self%l)
          do n = 1,n_pt              ! Do the exponential part of the gaussian
            x=pt(n,1) - self%pos(1); y=pt(n,2) - self%pos(2); z=pt(n,3) - self%pos(3)
            x2 = x*x;   y2 = y*y;   z2 = z*z
            rr = x2 + y2 + z2
            g0 = 0.0d0
            g1 = 0.0d0
            g2 = 0.0d0
            do p = 1,self%n_cc           ! Loop over primitives
              aa = self%ex(p)
              a2 = aa+aa
              val = self%cc(p)*exp(-aa*rr)
              g0 = g0 +    val
              g1 = g1 - a2*val
              g2 = g2 + a2*a2*val
            end do
            g2x = g2 * x2
            g2y = g2 * y2
            g2z = g2 * z2

             ! Some that are used multiple times.
             ! Taken out of loop over n_comp.
            gx0 = g1 + g2x
            gy0 = g1 + g2y
            gz0 = g1 + g2z
            gx1 = (3.0d0 * g1 + g2x)*x   ! *x^1
            gy1 = (3.0d0 * g1 + g2y)*y   ! *y^1
            gz1 = (3.0d0 * g1 + g2z)*z   ! *z^1

            do b = 1,self%n_comp         ! Loop over basis functions
                nx = nn(1,b)
                ny = nn(2,b)
                nz = nn(3,b)

                select case (nx)
                  case (0);     gnbx = gx0
                  case (1);     gnbx = gx1
                  case (2);     gnbx = g0+g0 + (5 * g1 + g2x) * x * x
                  case default
                    tn2 = x**(nx-2)
                    tn = tn2*x*x
                    gnbx = nx*(nx-1)*g0*tn2 + ((nx+nx+1) * g1 + g2x) * tn
                end select
                select case (ny)
                  case (0);     gnby = gy0
                  case (1);     gnby = gy1
                  case (2);     gnby = g0+g0 + (5 * g1 + g2y) * y * y
                  case default
                    tn2 = y**(ny-2)
                    tn = tn2*y*y
                    gnby = ny*(ny-1)*g0*tn2 + ((ny+ny+1) * g1 + g2y) * tn
                end select
                select case (nz)
                  case (0);     gnbz = gz0
                  case (1);     gnbz = gz1
                  case (2);     gnbz = g0+g0 + (5 * g1 + g2z) * z * z
                  case default
                    tn2 = z**(nz-2)
                    tn = tn2*z*z
                    gnbz = nz*(nz-1)*g0*tn2 + ((nz+nz+1) * g1 + g2z) * tn
                end select

                do j = 1,nx
                   gnby = x*gnby     ! Do the cartesian (x_i)^j, i/=k part of
                   gnbz = x*gnbz     ! gaussian power of the xyz part not equal
                end do ! to deriv. component k
                do j = 1,ny
                   gnbx = y*gnbx
                   gnbz = y*gnbz
                end do
                do j = 1,nz
                   gnbx = z*gnbx
                   gnby = z*gnby
                end do

                facb = fac(b)
                g(n,b,1) = gnbx*facb  ! Basis fn. normalisation factor
                g(n,b,2) = gnby*facb
                g(n,b,3) = gnbz*facb
             end do
          end do
          call destroy_(fac)
          call destroy_(nn)
      end select

   end subroutine

   subroutine make_laplacian_grid_1(self,g,h,i,pt)
    type(shell1_type) :: self
    ! Return "g(j,n,1:3)", the value of the second derivatives
    ! (d/dx2,d/dy2,d/dz2) of the shell component n on grid point j given a
    ! set of grid points "pt(j,1:3)". Also return "h(j,n,1:3)", the value of the
    ! first derivative, and return "i(n)", the value of the gaussian
      real(kind=kind(1.0d0)), dimension(:,:) :: pt
      real(kind=kind(1.0d0)), dimension(:,:,:) :: g,h
      real(kind=kind(1.0d0)), dimension(:,:) :: i
      real(kind=kind(1.0d0)), dimension(:), pointer :: fac
      integer(kind=kind(1)), dimension(:,:), pointer :: nn
      integer(kind=kind(1)) :: n_pt,n,b,p,j,nx,ny,nz
      real(kind=kind(1.0d0)) :: aa,a2,x,y,z,rr,val
      real(kind=kind(1.0d0)) :: g0,g1,g2,g2x,g2y,g2z
      real(kind=kind(1.0d0)) :: gnbx,gnby,gnbz,x2,y2,z2
      real(kind=kind(1.0d0)) :: gx0,gx1,gy0,gy1,gz0,gz1,facb,inb
      real(kind=kind(1.0d0)) :: hnbx,hnby,hnbz,hx1,hy1,hz1,h1x,h1y,h1z
      real(kind=kind(1.0d0)) :: tn,tn1,tn2,twog0

      call ensure_(tonto,size(pt,2)==3,"SHELL1:make_laplacian_grid_1 ... pt matrix incorrectly dimensioned")
      n_pt = size(pt,1)
      select case (self%l)
        case (0)
          do n = 1,n_pt              ! Do the exponential part of the gaussian
            x=pt(n,1) - self%pos(1); y=pt(n,2) - self%pos(2); z=pt(n,3) - self%pos(3)
            x2 = x*x;   y2 = y*y;   z2 = z*z
            rr = x2 + y2 + z2
            g0 = 0.0d0
            g1 = 0.0d0
            g2 = 0.0d0
            do p = 1,self%n_cc           ! Loop over primitives
              aa = self%ex(p)
              a2 = aa+aa
              val = self%cc(p)*exp(-aa*rr)
              g0 = g0 + val
              g1 = g1 - a2*val
              g2 = g2 + a2*a2*val
            end do
            g(n,1,1) = g1 + g2*x2
            g(n,1,2) = g1 + g2*y2
            g(n,1,3) = g1 + g2*z2
            h(n,1,1) = g1*x
            h(n,1,2) = g1*y
            h(n,1,3) = g1*z
            i(n,1)   = g0
          end do
        case (1)
          do n = 1,n_pt              ! Do the exponential part of the gaussian
            x=pt(n,1) - self%pos(1); y=pt(n,2) - self%pos(2); z=pt(n,3) - self%pos(3)
            x2 = x*x;   y2 = y*y;   z2 = z*z
            rr = x2 + y2 + z2
            g0 = 0.0d0
            g1 = 0.0d0
            g2 = 0.0d0
            do p = 1,self%n_cc           ! Loop over primitives
              aa = self%ex(p)
              a2 = aa+aa
              val = self%cc(p)*exp(-aa*rr)
              g0 = g0 + val
              g1 = g1 - a2*val
              g2 = g2 + a2*a2*val
            end do
            g2x = g2 * x2
            g2y = g2 * y2
            g2z = g2 * z2
            g(n,1,1) = x * (3 * g1 + g2x)
            g(n,1,2) = x * (g1 + g2y)
            g(n,1,3) = x * (g1 + g2z)
            g(n,2,1) = y * (g1 + g2x)
            g(n,2,2) = y * (3 * g1 + g2y)
            g(n,2,3) = y * (g1 + g2z)
            g(n,3,1) = z * (g1 + g2x)
            g(n,3,2) = z * (g1 + g2y)
            g(n,3,3) = z * (3 * g1 + g2z)
            h1x = g1*x
            h1y = g1*y
            h1z = g1*z
            h(n,1,1) = g0+x2*g1
            h(n,1,2) = x*h1y
            h(n,1,3) = x*h1z
            h(n,2,1) = y*h1x
            h(n,2,2) = g0+y2*g1
            h(n,2,3) = y*h1z
            h(n,3,1) = z*h1x
            h(n,3,2) = z*h1y
            h(n,3,3) = g0+z2*g1
            i(n,1)   = g0*x
            i(n,2)   = g0*y
            i(n,3)   = g0*z
          end do
        case default
          call create_(nn,3,self%n_comp);   call make_gaussian_xyz_powers_(self%l,nn)
          call create_(fac,self%n_comp);    call normalising_factors_(fac,self%l)
          do n = 1,n_pt              ! Do the exponential part of the gaussian
            x=pt(n,1) - self%pos(1); y=pt(n,2) - self%pos(2); z=pt(n,3) - self%pos(3)
            x2 = x*x;   y2 = y*y;   z2 = z*z
            rr = x2 + y2 + z2
            g0 = 0.0d0
            g1 = 0.0d0
            g2 = 0.0d0
            do p = 1,self%n_cc           ! Loop over primitives
              aa = self%ex(p)
              a2 = aa+aa
              val = self%cc(p)*exp(-aa*rr)
              g0 = g0 + val
              g1 = g1 - aa*val
              g2 = g2 + aa*aa*val
!              g1 = g1 - a2*val
!              g2 = g2 + a2*a2*val
            end do
            g1 = g1 * 2.0d0
            g2 = g2 * 4.0d0
            g2x = g2 * x2
            g2y = g2 * y2
            g2z = g2 * z2

             ! Some that are used multiple times.
             ! Taken out of loop over n_comp.
            gx0 = g1 + g2x
            gy0 = g1 + g2y
            gz0 = g1 + g2z
            gx1 = (3.0d0 * g1 + g2x)*x   ! *x^1
            gy1 = (3.0d0 * g1 + g2y)*y   ! *y^1
            gz1 = (3.0d0 * g1 + g2z)*z   ! *z^1
            h1x = g1*x
            h1y = g1*y
            h1z = g1*z
            hx1 = g0 + x2*g1
            hy1 = g0 + y2*g1
            hz1 = g0 + z2*g1

            do b = 1,self%n_comp         ! Loop over basis functions
                inb = g0
                nx = nn(1,b)
                ny = nn(2,b)
                nz = nn(3,b)

                select case (nx)
                  case (0);     gnbx = gx0
                                hnbx = h1x
                  case (1);     gnbx = gx1
                                hnbx = hx1
                  case (2)
                    tn = x*x
                    twog0 = g0+g0
                    gnbx = twog0 + (5 * g1 + g2x)*tn
                    hnbx = twog0*x + h1x*tn
                  case default
                    tn2 = x**(nx-2)
                    tn1 = tn2*x
                    tn = tn2*x*x
                    gnbx = nx*(nx-1)*g0*tn2 + ((2*nx+1) * g1 + g2x)*tn
                    hnbx = nx*g0*tn1 + h1x*tn
                end select
                select case (ny)
                  case (0);     gnby = gy0
                                hnby = h1y
                  case (1);     gnby = gy1
                                hnby = hy1
                  case (2)
                    tn = y*y
                    twog0 = g0+g0
                    gnby = twog0 + (5 * g1 + g2y)*tn
                    hnby = twog0*y + h1y*tn
                  case default
                    tn2 = y**(ny-2)
                    tn1 = tn2*y
                    tn = tn2*y*y
                    gnby = ny*(ny-1)*g0*tn2 + ((2*ny+1) * g1 + g2y)*tn
                    hnby = ny*g0*tn1 + h1y*tn
                end select
                select case (nz)
                  case (0);     gnbz = gz0
                                hnbz = h1z
                  case (1);     gnbz = gz1
                                hnbz = hz1
                  case (2)
                    tn = z*z
                    twog0 = g0+g0
                    gnbz = twog0 + (5 * g1 + g2z)*tn
                    hnbz = twog0*z + h1z*tn
                  case default
                    tn2 = z**(nz-2)
                    tn1 = tn2*z
                    tn = tn2*z*z
                    gnbz = nz*(nz-1)*g0*tn2 + ((2*nz+1) * g1 + g2z)*tn
                    hnbz = nz*g0*tn1 + h1z*tn
                end select

                do j = 1,nx
                   gnby = x*gnby     ! Do the cartesian (x_i)^j, i/=k part of
                   gnbz = x*gnbz     ! gaussian power of the xyz part not equal
                   hnby = x*hnby
                   hnbz = x*hnbz
                   inb  = x*inb
                end do ! to deriv. component k
                do j = 1,ny
                   gnbx = y*gnbx
                   gnbz = y*gnbz
                   hnbx = y*hnbx
                   hnbz = y*hnbz
                   inb  = y*inb
                end do
                do j = 1,nz
                   gnbx = z*gnbx
                   gnby = z*gnby
                   hnbx = z*hnbx
                   hnby = z*hnby
                   inb  = z*inb
                end do

                facb = fac(b)
                g(n,b,1) = gnbx*facb  ! Basis fn. normalisation factor
                g(n,b,2) = gnby*facb
                g(n,b,3) = gnbz*facb
                h(n,b,1) = hnbx*facb
                h(n,b,2) = hnby*facb
                h(n,b,3) = hnbz*facb
                i(n,b)   = inb *facb
             end do
          end do
          call destroy_(fac)
          call destroy_(nn)
      end select

   end subroutine

end
